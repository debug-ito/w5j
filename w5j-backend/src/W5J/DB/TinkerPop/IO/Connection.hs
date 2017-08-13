-- |
-- Module: W5J.DB.TinkerPop.IO.Connection
-- Description: Connection and IO with the DB
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- It wraps the gremlin-haskell package.
module W5J.DB.TinkerPop.IO.Connection
       ( Connection,
         Gremlin,
         Binding,
         withConnection,
         submit
       ) where

import Data.Aeson (Value)

import Database.TinkerPop.Types (Connection, Gremlin, Binding)
import qualified Database.TinkerPop as TP

-- | Make a 'Connection' to the given server and run the given action.
withConnection :: String
               -- ^ server hostname
               -> Int
               -- ^ server port number
               -> (Connection -> IO ())
               -- ^ action
               -> IO ()
withConnection = TP.run

submit :: Connection -> Gremlin -> Maybe Binding -> IO (Either String [Value])
submit = TP.submit
