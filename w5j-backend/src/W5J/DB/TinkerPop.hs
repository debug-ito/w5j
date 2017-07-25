{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module: W5J.DB.TinkerPop
-- Description: DB backend for TinkerPop Gremlin Server
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module W5J.DB.TinkerPop
       ( Connection,
         withConnection,
         addWhat
       ) where

import Control.Monad.Trans.State (State)
import qualified Control.Monad.Trans.State as State
import Data.Aeson (ToJSON(toJSON), Value)
import qualified Data.HashMap.Strict as HM
import Data.Monoid ((<>), mconcat)
import Data.Text (pack, Text)
import qualified Data.Text as T
import Database.TinkerPop.Types
  ( Connection, Binding
  )
import qualified Database.TinkerPop as TP

import W5J.Time (currentTime, toEpochMsec)
import W5J.What (What(..), WhatID)
import W5J.When (When(..))

-- | Make a 'Connection' to the given server and run the given action.
withConnection :: String
               -- ^ server hostname
               -> Int
               -- ^ server port number
               -> (Connection -> IO ())
               -- ^ action
               -> IO ()
withConnection = TP.run


-- How to configure transactions in the remote Gremlin Server?

-- when I connect to the remote server from gremlin.sh, it seems
-- transactions are not enabled. Every operation seems immediately
-- commited.

-- https://groups.google.com/forum/#!topic/gremlin-users/S4T9yOfHlV4
-- http://tinkerpop.apache.org/docs/3.0.1-incubating/#sessions
-- どうやらデフォルトで"sessionless"で動作するらしい。要はauto commitモード。
--
-- sessionを使うためにはリクエストの"processor"を"session"にして、
-- "eval"オペレーションの"session"フィールドにUUIDを詰めないといけない。
-- この機能はgremlin-haskellにはない。。
--
-- でもひとつのリクエストで複文書けるな。


-- | Add a new 'What' vertex into the DB.
addWhat :: Connection
        -> What
        -- ^ 'What' vertex to add. 'whatId', 'whatCreatedAt' and
        -- 'whatUpdatedAt' fields are ignored, and set automatically.
        -> IO (WhatID)
        -- ^ newly created ID for 'whatId' field.
addWhat conn what = do
  cur_time <- currentTime
  handleResult =<< TP.submit conn gremlin (Just $ binds cur_time)
  where
    gremlin = "g.addV(label, 'what', "
              <> "'title', TITLE, "
              <> "'body', BODY, "
              <> gremlin_tags
              <> "'created_at', CREATED_AT, "
              <> "'updated_at', UPDATED_AT).id()"
    indices :: [Int]
    indices = [0 ..]
    indexed_tags = zip indices $ whatTags what
    tagsVar i = "TAGS" <> (pack $ show i)
    gremlin_tags = mconcat $ map (\(i, _) -> "'tags', " <> tagsVar i <> ", ") $ indexed_tags
    binds cur_time = HM.fromList
                     ( [ ("TITLE", toJSON $ whatTitle $ what),
                         ("BODY", toJSON $ whatBody $ what),
                         ("CREATED_AT", toJSON $ cur_time_msec),
                         ("UPDATED_AT", toJSON $ cur_time_msec)
                       ]
                       ++ binds_tags
                     )
      where
        cur_time_msec = toEpochMsec cur_time
    binds_tags = map (\(i, tag) -> (tagsVar i, toJSON tag)) indexed_tags
    handleResult (Left err) = error err -- todo
    handleResult (Right ret) = do
      print ret  --- > [Number 8352.0]
      return 0 -- todo


type VertexID = Integer

-- | Add a 'When' vertex.
addWhen :: Connection -> When -> IO VertexID
addWhen conn wh = handleResult =<< TP.submit conn gremlin (Just binds)
  where
    gremlin = "g.addV(label, 'when', "
              <> "'instant', INSTANT, "
              <> "'is_time_explicit', IS_TIME_EXPLICIT, "
              <> "'time_zone', TIME_ZONE).id()"
    binds = HM.fromList
            [ ("INSTANT", toJSON $ toEpochMsec $ whenInstant wh),
              ("IS_TIME_EXPLICIT", toJSON $ whenIsTimeExplicit wh),
              ("TIME_ZONE", toJSON $ dummy_tz) -- TODO
            ]
    dummy_tz :: String
    dummy_tz = "DUMMY_TZ"
    handleResult (Left err) = error err -- todo
    handleResult (Right ret) = do
      print ret
      return 0 -- TODO

-- addWhenじゃダメか。複文をたたきつけないといけない。

type PlaceHolderIndex = Int

type GBuilder = State (PlaceHolderIndex, [Value])

newPlaceHolder :: Value -> GBuilder PlaceHolderIndex
newPlaceHolder val = do
  (next_index, values) <- State.get
  State.put (succ next_index, values ++ [val])
  return next_index

place :: PlaceHolderIndex -> Text
place index = "v" <> (pack $ show index)

addVertexSentence :: Text
                  -- ^ vertex label
                  -> Maybe Text
                  -- ^ variable name to receive the result
                  -> [(Text, Value)]
                  -- ^ properties
                  -> GBuilder Text
                  -- ^ gremlin script
addVertexSentence label mreceiver props = gremlin
  where
    gremlin = case mreceiver of
      Nothing -> gremlin_addV
      Just receiver -> do
        add_sentence <- gremlin_addV
        return (receiver <> " = " <> add_sentence)
    gremlin_addV = do
      var_label <- fmap place $ newPlaceHolder $ toJSON $ label
      props_gremlin <- fmap (mconcat . map pairToGremlin) $ mapM toVarNamePair props
      return ("graph.addVertex(label, " <> var_label <> props_gremlin <> ")")
    toVarNamePair (prop_name, prop_val) = do
      var_name <- fmap place $ newPlaceHolder prop_val
      return (prop_name, var_name)
    pairToGremlin (prop_name, var_name) =
      ", '" <> prop_name <> "', " <> var_name


