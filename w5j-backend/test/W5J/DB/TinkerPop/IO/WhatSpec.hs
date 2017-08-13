{-# LANGUAGE OverloadedStrings #-}
module W5J.DB.TinkerPop.IO.WhatSpec (main,spec) where

import Control.Monad (mapM_)
import Data.List (nub)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Test.Hspec

import W5J.Interval ((...), mapInterval)
import W5J.Time
  ( zeroTime, toEpochMsec, fromEpochMsec,
    tzToString, tzFromString
  )
import W5J.What (What(..))
import W5J.When (When(..), currentUTCWhen)
import W5J.Where (Where(..))
import W5J.DB.TinkerPop.IO.What (addWhat, getWhatById, queryWhat)
import W5J.DB.TinkerPop.Query.Common
  ( Query(..), QCondTree(..), QOrder(..), QRange, qRange
  )
import W5J.DB.TinkerPop.Query.What (QOrderBy(..), QCond(..))


import W5J.DB.TinkerPop.TestUtil (withEnv, withCleanDB)

main :: IO ()
main = hspec spec

expectJust :: Maybe a -> IO a
expectJust (Just a) = return a
expectJust Nothing = do
  expectationFailure "expecting Just, but got Nothing"
  undefined

expectField :: (b -> b -> Expectation) -> a -> a -> (a -> b) -> Expectation
expectField assertion got expected accessor =
  assertion (accessor got) (accessor expected)

toWhenInDB :: When -> When
toWhenInDB w = w { whenInstant = fromEpochMsec $ toEpochMsec $ whenInstant w,
                   whenTimeZone = fromJust $ tzFromString $ tzToString $ whenTimeZone w
                 }

whatWhereNames :: What -> [Text]
whatWhereNames = map whereName . whatWheres

spec :: Spec
spec = do
  withEnv $ do
    spec_add_get
    spec_queryWhat

spec_add_get :: SpecWith (String, Int)
spec_add_get = describe "addWhat, getWhatById" $ do
  it "should add and get What data" $ withCleanDB $ \conn -> do
    cur_when <- currentUTCWhen
    let input_what = what cur_when
    wid <- addWhat conn input_what
    got_what <- expectJust =<< getWhatById conn wid
    let exp_what = expectedWhat input_what
        fieldEq :: (Eq a, Show a) => (What -> a) -> Expectation
        fieldEq = expectField shouldBe got_what exp_what
        fieldMatchList :: (Eq a, Show a) => (What -> [a]) -> Expectation
        fieldMatchList = expectField shouldMatchList got_what exp_what
    fieldEq whatTitle
    fieldEq whatWhen
    fieldMatchList whatWhereNames
    fieldEq whatBody
    fieldMatchList whatTags
  where
    what cur_when = What { whatId = 0,
                           whatTitle = "whaaat title",
                           whatWhen = Just (cur_when ... cur_when),
                           whatWheres = wheres,
                           whatBody = "whaat body",
                           whatTags = ["foo", "bar", "buzz", "foo"],
                           whatCreatedAt = zeroTime,
                           whatUpdatedAt = zeroTime
                         }
    expectedWhat input_what =
      input_what { whatWhen = fmap (mapInterval toWhenInDB) $ whatWhen input_what,
                   whatTags = nub $ whatTags input_what
                 }
    wheres = [ Where Nothing "place 1",
               Where Nothing "place 999"
             ]

spec_queryWhat :: SpecWith (String, Int)
spec_queryWhat = describe "queryWhat" $ do
  specify "order" $ withCleanDB $ \conn -> do
    addSampleWhats conn
    let q = Query { queryCond = QCondTrue,
                    queryOrder = QOrderAsc,
                    queryOrderBy = QOrderByWhen,
                    queryRange = qRange 0 3
                  }
    got <- queryWhat conn q
    True `shouldBe` False -- TODO
  where
    addSampleWhats conn = mapM_ (addWhat conn) sampleWhats
    sampleWhats :: [What]
    sampleWhats =
      [ -- TODO
      ]

    

