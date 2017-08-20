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

import Control.Monad (void)
import W5J.DB.TinkerPop.IO.Connection (Connection)
import W5J.DB.TinkerPop.Error (toGremlinError)
import W5J.DB.TinkerPop.GBuilder (submitGBuilder)
import W5J.DB.TinkerPop.GScript (gRaw)

-- | Clear all vertices and edges.
clearAll :: Connection -> IO ()
clearAll conn = void $ toGremlinError =<< (submitGBuilder conn $ return $ gRaw "g.V().drop()")
