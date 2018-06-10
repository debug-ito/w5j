{-# LANGUAGE TypeFamilies #-}
-- |
-- Module: W5J.DB.TinkerPop.IO.Connection
-- Description: Connection and IO with the DB
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- It wraps the gremlin-haskell package.
module W5J.DB.TinkerPop.IO.Connection
       ( Connection,
         Binding,
         withConnection,
         submit,
         submitBinder
       ) where

import Control.Exception.Safe (bracket)
import Data.Aeson (Value, Object)
import Data.Foldable (toList)
import qualified Data.HashMap.Strict as HM
import Data.Greskell
  ( Binder, Greskell, runBinder, Binding, toGremlin,
    ToGreskell(..)
  )
import Data.Greskell.AsIterator (AsIterator(IteratorItem))
import Data.Greskell.GraphSON (FromGraphSON)
import Data.Text (Text)
import Network.Greskell.WebSocket.Client
  ( Client, connect, close
  )
import qualified Network.Greskell.WebSocket.Client as WS

type Connection = Client

-- | Make a 'Connection' to the given server and run the given action.
withConnection :: String
               -- ^ server hostname
               -> Int
               -- ^ server port number
               -> (Connection -> IO ())
               -- ^ action
               -> IO ()
withConnection host port = bracket (connect host port) close

submit :: (ToGreskell g, a ~ GreskellReturn g, AsIterator a, e ~ IteratorItem a, FromGraphSON e)
       => Connection -> g -> Maybe Binding -> IO [e]
submit c g b = fmap toList $ WS.slurpResults =<< WS.submit c g b

submitBinder :: (ToGreskell g, a ~ GreskellReturn g, AsIterator a, e ~ IteratorItem a, FromGraphSON e)
             => Connection -> Binder g -> IO [e]
submitBinder conn binder = submit conn g mbinding
  where
    (g, binding_map) = runBinder binder
    mbinding = if HM.null binding_map
               then Nothing
               else Just binding_map

