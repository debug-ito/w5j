module W5J.TimeSpec (main,spec) where

import Test.Hspec
import Test.QuickCheck (property)

import W5J.Time (toEpochMsec, fromEpochMsec)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "toEpochTime, fromEpochTime" $ do
  it "should isomorphic" $ property $ \msec ->
    (toEpochMsec . fromEpochMsec) msec `shouldBe` msec
    
