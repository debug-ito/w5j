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
         submit,
         submitBinder
       ) where

import Data.Aeson (Value)
import qualified Data.HashMap.Strict as HM
import Data.Greskell
  ( Binder, Greskell, runBinder, Binding, toGremlin,
    ToGreskell(..)
  )

import Database.TinkerPop.Types (Connection, Gremlin)
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

submitBinder :: ToGreskell g => Connection -> Binder g -> IO (Either String [Value])
submitBinder conn binder = submit conn gremlin mbinding
  where
    (g, binding_map) = runBinder binder
    gremlin = toGremlin g
    mbinding = if HM.null binding_map
               then Nothing
               else Just binding_map

