-- |
-- Module: W5J.DB.TinkerPop.Error
-- Description: Error types
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module W5J.DB.TinkerPop.Error
       ( DBError(..),
         toGremlinError,
         parseError,
         ioFromJSON
       ) where

import Control.Exception (Exception, throwIO)
import Data.Aeson
  ( FromJSON, fromJSON, Result(Error,Success),
    Value
  )

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

parseError :: String -> IO a
parseError err = throwIO $ ParseError err

ioFromJSON :: FromJSON a => Value -> IO a
ioFromJSON = toError . fromJSON
  where
    toError (Error err) = parseError err
    toError (Success a) = return a
