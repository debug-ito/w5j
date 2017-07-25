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

import Data.Aeson (ToJSON(toJSON))
import qualified Data.HashMap.Strict as HM
import Data.Monoid ((<>), mconcat)
import Data.Text (pack)
import Database.TinkerPop.Types
  ( Connection
  )
import qualified Database.TinkerPop as TP

import W5J.Time (currentTime, toEpochMsec)
import W5J.What (What(..), WhatID)

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
