{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module: W5J.DB.TinkerPop.IO.Misc
-- Description: other functions for IO with the DB
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module W5J.DB.TinkerPop.IO.Misc
       ( clearAll
       ) where

import Data.Greskell (unsafeGreskell)

import Control.Monad (void)
import W5J.DB.TinkerPop.IO.Connection (Connection, submitBinder)
import W5J.DB.TinkerPop.Error (toGremlinError)

-- | Clear all vertices and edges.
clearAll :: Connection -> IO ()
clearAll conn = void $ toGremlinError =<< (submitBinder conn $ return $ unsafeGreskell "g.V().drop()")
