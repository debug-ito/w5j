module W5J.DB.TinkerPop.TestUtil
       ( -- withEnvConnection
         withEnv,
         withCleanDB
       ) where

import System.Environment (lookupEnv)
import Test.Hspec (SpecWith, Spec, pendingWith, before)

import W5J.DB.TinkerPop (Connection, withConnection, clearAll)

requireEnv :: String -> IO String
requireEnv env_key = maybe bail return =<< lookupEnv env_key
  where
    bail = pendingWith msg >> return ""
      where
        msg = "Set environment variable "++ env_key ++ " for DB test. "
              ++ "Note that the testes erase the entire database..."

withEnv :: SpecWith (String, Int) -> Spec
withEnv = before $ do
  hostname <- requireEnv "W5J_TINKERPOP_HOST_TEST"
  port <- fmap read $ requireEnv "W5J_TINKERPOP_PORT_TEST"
  return (hostname, port)

withCleanDB :: (Connection -> IO ()) -> (String, Int) -> IO ()
withCleanDB act (host, port) = withConnection host port (\conn -> clearAll conn >> act conn)
