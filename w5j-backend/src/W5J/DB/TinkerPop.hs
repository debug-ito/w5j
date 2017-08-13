{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module: W5J.DB.TinkerPop
-- Description: DB backend for TinkerPop Gremlin Server
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module W5J.DB.TinkerPop
       ( -- * Connection
         Connection,
         withConnection,
         -- * What
         addWhat,
         updateWhat,
         getWhatById,
         queryWhat,
         deleteWhat,
         -- * Clear
         clearAll
       ) where

import Control.Monad (void, mapM)
import Data.Maybe (listToMaybe)
import Data.Monoid ((<>), mconcat)

import W5J.DB.TinkerPop.Error (toGremlinError, parseError)
import W5J.DB.TinkerPop.GBuilder (newPlaceHolder, submitGBuilder)
import W5J.DB.TinkerPop.Parse (ioFromJSON, unACompleteWhat)
import qualified W5J.DB.TinkerPop.Query.What as QueryWhat
import W5J.Aeson (toAWhat)
import W5J.Interval (inf, sup)
import W5J.Time (currentTime, toEpochMsec)
import W5J.What (What(..), WhatID)
import W5J.When (When(..))


