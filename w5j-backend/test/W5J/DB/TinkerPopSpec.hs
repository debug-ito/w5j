{-# LANGUAGE OverloadedStrings #-}
module W5J.DB.TinkerPopSpec (main,spec) where

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
import W5J.DB.TinkerPop (addWhat, getWhatById)

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
spec = withEnv $ describe "addWhat, getWhatById"
       $ it "should add and get What data" $ withCleanDB $ \conn -> do
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

