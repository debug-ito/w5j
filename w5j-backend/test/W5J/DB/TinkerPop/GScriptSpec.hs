{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module W5J.DB.TinkerPop.GScriptSpec (main,spec) where

import Data.String (fromString)
import Data.Text (Text, pack)
import Test.Hspec
import Test.QuickCheck (property, Arbitrary(..))

import W5J.DB.TinkerPop.GScript
  ( gRaw, getGScript, gLiteral,
    gPlaceHolder, toPlaceHolderVariable
  )

-- TODO: move this into a single support module.
instance Arbitrary Text where
  arbitrary = fmap pack arbitrary

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "gRaw" $ it "should be just a raw script text" $ property $ \t ->
    (getGScript $ gRaw t) `shouldBe` t
  describe "gPlaceHolder" $ it "should create a placeholder variable" $ property $ \i ->
    (getGScript $ gPlaceHolder i) `shouldBe` toPlaceHolderVariable i
  describe "gLateral and fromString" $ do
    specify "empty" $ checkLiteral "" "\"\""
    specify "words" $ checkLiteral "hoge foo bar"  "\"hoge foo bar\""
    specify "escaped" $ checkLiteral "foo 'aaa \n \t \\ \"bar\"" "\"foo 'aaa \\n \\t \\\\ \\\"bar\\\"\""


checkLiteral :: String -> Text -> Expectation
checkLiteral input expected = do
  (getGScript $ fromString input) `shouldBe` expected
  (getGScript $ gLiteral $ pack input) `shouldBe` expected
