-- |
-- Module: W5J.DB.TinkerPop.Error
-- Description: Error types
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module W5J.DB.TinkerPop.Error
       ( DBError(..),
         toGremlinError
       ) where

import Control.Exception (Exception, throwIO)

-- | The exception type for DB.TinkerPop.
data DBError
  = GremlinError String
  -- ^ Error from gremlin driver.
  | ParseError String
  -- ^ Error in parsing the result.
  deriving (Show,Eq,Ord)

instance Exception DBError

toGremlinError :: IO (Either String a) -> IO a
toGremlinError action = throwLeft =<< action
  where
    throwLeft (Left err) = throwIO $ GremlinError err
    throwLeft (Right r) = return r
