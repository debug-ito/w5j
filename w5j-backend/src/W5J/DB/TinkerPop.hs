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
  let (gremlin, binds) = gremlinAndBinding cur_time
  handleResult =<< TP.submit conn (gremlin <> ".id()") (Just binds)
  where
    props cur_time = [ ("title", toJSON $ whatTitle $ what),
                       ("body", toJSON $ whatBody $ what),
                       ("created_at", toJSON $ cur_time_msec),
                       ("updated_at", toJSON $ cur_time_msec)
                     ]
                     ++ (map (\t -> ("tags", toJSON t)) $ whatTags what)
      where
        cur_time_msec = toEpochMsec cur_time
    gremlinAndBinding cur_time =
      runGBuilder $ addVertexSentence "what" Nothing (props cur_time)
    handleResult (Left err) = error err -- todo
    handleResult (Right ret) = do
      print ret  --- > [Number 8352.0]
      return 0 -- todo


addWhenSentence :: Maybe Text
                -- ^ variable name to receive the result
                -> When
                -> GBuilder Text
addWhenSentence mreceiver wh = addVertexSentence "when" mreceiver props
  where
    props = [ ("instant", toJSON $ toEpochMsec $ whenInstant wh),
              ("is_time_explicit", toJSON $ whenIsTimeExplicit wh),
              ("time_zone", toJSON $ dummy_tz) -- TODO
            ]
    dummy_tz :: String
    dummy_tz = "DUMMY_TZ"

type PlaceHolderIndex = Int

type GBuilder = State (PlaceHolderIndex, [Value])

newPlaceHolder :: Value -> GBuilder PlaceHolderIndex
newPlaceHolder val = do
  (next_index, values) <- State.get
  State.put (succ next_index, values ++ [val])
  return next_index

place :: PlaceHolderIndex -> Text
place index = "v" <> (pack $ show index)

runGBuilder :: GBuilder a -> (a, Binding)
runGBuilder gbuilder = (ret, binding)
  where
    (ret, (_, values)) = State.runState gbuilder (0, [])
    binding = HM.fromList $ zip (map place [0 ..]) $ values

seqGremlin :: [GBuilder Text] -> GBuilder Text
seqGremlin = fmap seqSentences . sequence
  where
    seqSentences = T.intercalate "; "

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


