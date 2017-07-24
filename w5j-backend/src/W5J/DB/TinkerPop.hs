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

import Database.TinkerPop.Types
  ( Connection
  )
import qualified Database.TinkerPop as TP

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

-- | Add a new 'What' vertex into the DB.
addWhat :: Connection
        -> What
        -- ^ 'What' vertex to add. 'whatId' field is ignored.
        -> IO (WhatID)
        -- ^ newly created ID for 'whatId' field.
addWhat = undefined

