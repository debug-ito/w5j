{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module W5J.AesonSpec (main,spec) where

import Data.Aeson (decode, encode, Value, fromJSON, Result(..))
import Data.Maybe (fromJust)
import Test.Hspec
import Text.Heredoc (here)

import W5J.Aeson (toAWhat, fromAWhat)
import W5J.Interval ((...))
import W5J.What (What(..))
import W5J.When (When(..))
import W5J.Where (Where(..))
import W5J.Time (fromEpochMsec, tzFromString)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  spec_AWhat

sampleWhat :: What
sampleWhat =
  What { whatId = 999,
         whatTitle = "hoge title",
         whatWhen = Just (when_from ... when_to),
         whatWheres = wheres,
         whatBody = "hoge body",
         whatTags = ["foo", "bar"],
         whatCreatedAt = fromEpochMsec 999,
         whatUpdatedAt = fromEpochMsec 1999
       }
  where
    when_from = When { whenInstant = fromEpochMsec 1000,
                       whenIsTimeExplicit = False,
                       whenTimeZone = fromJust $ tzFromString "+0900"
                     }
    when_to = When { whenInstant = fromEpochMsec 2000,
                     whenIsTimeExplicit = True,
                     whenTimeZone = fromJust $ tzFromString "+0900"
                   }
    wheres = [ Where { whereId = Just 5, whereName = "place 5" },
               Where { whereId = Nothing , whereName = "new place" }
             ]

sampleJSON :: Value
sampleJSON = fromJust $ decode $ [here|
{
  "what_id": 999,
  "title": "hoge title",
  "when": {
    "from": {"instant": 1000, "is_time_explicit": false, "time_zone": "+0900"},
    "to":   {"instant": 2000, "is_time_explicit": true, "time_zone": "+0900"}
  },
  "wheres": [
    {"where_id": 5, "name": "place 5"},
    {"where_id": null, "name": "new place"}
  ],
  "body": "hoge body",
  "tags": ["foo", "bar"],
  "created_at": 999,
  "updated_at": 1999
}
|]

spec_AWhat :: Spec
spec_AWhat = describe "AWhat" $ do
  specify "toAWhat" $ do
    (decode $ encode $ toAWhat $ sampleWhat) `shouldBe` (Just sampleJSON)
  specify "fromAWhat" $ do
    (fmap fromAWhat $ fromJSON $ sampleJSON) `shouldBe` (Success sampleWhat)
