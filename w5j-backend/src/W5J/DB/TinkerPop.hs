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
         addWhat,
         updateWhat,
         getWhatById,
         deleteWhat,
         clearAll
       ) where

import Control.Monad (void)
import Data.Maybe (listToMaybe)
import Data.Monoid ((<>), mconcat)
import Database.TinkerPop.Types
  ( Connection
  )
import qualified Database.TinkerPop as TP

import W5J.DB.TinkerPop.Error (toGremlinError, parseError)
import W5J.DB.TinkerPop.GBuilder (newPlaceHolder, submitGBuilder)
import W5J.DB.TinkerPop.Parse (ioFromJSON, unACompleteWhat)
import W5J.Aeson (toAWhat)
import W5J.Interval (inf, sup)
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


-- We (sometimes) use multiple Gremlin sentences in a single
-- request. This is because by default each request is enclosed in a
-- transaction. TinkerPop allows transactions over multiple requests
-- by means of "session", but gremlin-haskell (as of 0.1.0.2) does not
-- support it.
--
-- See:
--
-- - https://groups.google.com/forum/#!topic/gremlin-users/S4T9yOfHlV4
-- - http://tinkerpop.apache.org/docs/3.0.1-incubating/#sessions

-- | Add a new 'What' vertex into the DB.
addWhat :: Connection
        -> What
        -- ^ 'What' vertex to add. 'whatId', 'whatCreatedAt' and
        -- 'whatUpdatedAt' fields are ignored, and set automatically.
        -> IO (WhatID)
addWhat conn what = do
  cur_time <- currentTime
  parseResult =<< toGremlinError =<< (submitGBuilder conn $ getGBuilder $ setCurrentTime cur_time what)
  where
    setCurrentTime t w = w { whatCreatedAt = t,
                             whatUpdatedAt = t
                           }
    getGBuilder w = do
      p <- newPlaceHolder $ toAWhat w
      return ("addWhat(" <> p <> ").id()")
    parseResult [] = parseError "No element in the result."
    parseResult (ret : _) = ioFromJSON ret
    
-- | Update an existing 'What' vertex specified by the 'whatId' field.
updateWhat :: Connection
           -> What
           -- ^ 'whatUpdatedAt' field is ignored and set
           -- automatically.
           -> IO ()
updateWhat = undefined
-- TODO. We can just delete and re-create When vertices.

-- | Get 'What' vertex with the given 'WhatID'.
getWhatById :: Connection -> WhatID -> IO (Maybe What)
getWhatById conn wid = do
  mgot_val <- fmap (listToMaybe) $ toGremlinError =<< submitGBuilder conn gbuilder
  case mgot_val of
   Nothing -> return Nothing
   Just got_val -> fmap (Just . unACompleteWhat) $ ioFromJSON got_val
  where
    gbuilder = do
      v_wid <- newPlaceHolder wid
      return ("g.V(" <> v_wid <> ").hasLabel('what').map({ getCompleteWhat(it.get()) })")

-- | Delete a 'What' vertex.
deleteWhat :: Connection -> WhatID -> IO ()
deleteWhat = undefined
-- TODO. how should we treat other vertices connected (directly or
-- non-directly) connected to the deleted vertex?

-- | Clear all vertices and edges.
clearAll :: Connection -> IO ()
clearAll conn = void $ toGremlinError =<< TP.submit conn "g.V().drop()" Nothing

