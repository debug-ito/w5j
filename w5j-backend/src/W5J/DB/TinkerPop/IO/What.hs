{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module: W5J.DB.TinkerPop.IO.What
-- Description: IO about What data
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module W5J.DB.TinkerPop.IO.What
       ( addWhat,
         updateWhat,
         getWhatById,
         queryWhat,
         deleteWhat
       ) where

import Control.Monad (mapM)
import Data.Maybe (listToMaybe)
import Data.Monoid ((<>))

import W5J.Aeson (toAWhat)
import W5J.DB.TinkerPop.Error (toGremlinError, parseError)
import W5J.DB.TinkerPop.GBuilder (newPlaceHolder, submitGBuilder)
import W5J.DB.TinkerPop.IO.Connection (Connection)
import W5J.DB.TinkerPop.Parse (ioFromJSON, unACompleteWhat)
import qualified W5J.DB.TinkerPop.Query.What as QueryWhat
import W5J.Time (currentTime, toEpochMsec)
import W5J.What (What(..), WhatID)
import W5J.When (When(..))


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

queryWhat :: Connection -> QueryWhat.QueryWhat -> IO [What]
queryWhat conn query =
  fmap (map unACompleteWhat) $ mapM ioFromJSON =<< toGremlinError =<< submitGBuilder conn gbuilder
  where
    gbuilder = do
      query_gremlin <- QueryWhat.buildQuery query
      return (query_gremlin <> ".map({ getCompleteWhat(it.get()) })")

-- | Delete a 'What' vertex.
deleteWhat :: Connection -> WhatID -> IO ()
deleteWhat = undefined
-- TODO. how should we treat other vertices connected (directly or
-- non-directly) connected to the deleted vertex?

