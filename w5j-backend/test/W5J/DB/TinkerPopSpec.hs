{-# LANGUAGE OverloadedStrings #-}
module W5J.DB.TinkerPopSpec (main,spec) where

import Test.Hspec

import W5J.Interval ((...))
import W5J.Time (zeroTime)
import W5J.What (What(..))
import W5J.When (currentUTCWhen)
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

spec :: Spec
spec = withEnv $ describe "addWhat, getWhatById"
       $ it "should add and get What data" $ withCleanDB $ \conn -> do
         cur_when <- currentUTCWhen
         let input_what = what cur_when
         wid <- addWhat conn input_what
         got_what <- expectJust =<< getWhatById conn wid
         whatTitle got_what `shouldBe` whatTitle input_what
         -- TODO: 他のフィールドもテストする。あと、DBをクリアしないといけない。
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
    wheres = [ Where Nothing "place 1",
               Where Nothing "place 999"
             ]

