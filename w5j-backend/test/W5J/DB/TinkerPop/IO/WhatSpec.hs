{-# LANGUAGE OverloadedStrings #-}
module W5J.DB.TinkerPop.IO.WhatSpec (main,spec) where

import Control.Monad (mapM_)
import Data.List (nub)
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import W5J.Interval ((...), mapInterval)
import W5J.Time
  ( zeroTime, toEpochMsec, fromEpochMsec,
    tzToString, tzFromString
  )
import W5J.What (What(..))
import W5J.When (When(..), currentUTCWhen)
import W5J.Where (Where(..))
import W5J.DB.TinkerPop.IO.Connection (Connection)
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

makeWhen :: Integer -> Bool -> String -> When
makeWhen ins ex tz = When { whenInstant = fromEpochMsec ins,
                            whenIsTimeExplicit = ex,
                            whenTimeZone = fromJust $ tzFromString tz
                          }

checkWhat :: What -> What -> Expectation
checkWhat got_what input_what = do
  fieldEq whatTitle
  fieldEq whatWhen
  fieldMatchList whatWhereNames
  fieldEq whatBody
  fieldMatchList whatTags
  where
    exp_what = input_what { whatWhen = fmap (mapInterval toWhenInDB) $ whatWhen input_what,
                            whatTags = nub $ whatTags input_what
                          }
    fieldEq :: (Eq a, Show a) => (What -> a) -> Expectation
    fieldEq = expectField shouldBe got_what exp_what
    fieldMatchList :: (Eq a, Show a) => (What -> [a]) -> Expectation
    fieldMatchList = expectField shouldMatchList got_what exp_what

checkAddGet :: Connection -> What -> Expectation
checkAddGet conn input = do
  wid <- addWhat conn input
  got <- expectJust =<< getWhatById conn wid
  checkWhat got input

addWhats :: Connection -> [What] -> IO ()
addWhats conn ws = mapM_ (addWhat conn) ws


spec :: Spec
spec = do
  withEnv $ do
    spec_add_get
    spec_queryWhat

spec_add_get :: SpecWith (String, Int)
spec_add_get = describe "addWhat, getWhatById" $ do
  specify "has when, tags and wheres" $ withCleanDB $ \conn -> do
    cur_when <- currentUTCWhen
    let input_what =
          What { whatId = 0,
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
    checkAddGet conn input_what
  specify "no when, no tag, no where" $ withCleanDB $ \conn -> do
    let input_what =
          What { whatId = 0,
                 whatTitle = "foo bar",
                 whatWhen = Nothing,
                 whatWheres = [],
                 whatBody = "body",
                 whatTags = [],
                 whatCreatedAt = zeroTime,
                 whatUpdatedAt = zeroTime
               }
    checkAddGet conn input_what
    

spec_queryWhat :: SpecWith (String, Int)
spec_queryWhat = describe "queryWhat" $ do
  spec_queryWhat_no_cond
  spec_queryWhat_tag
  spec_queryWhat_term


spec_queryWhat_no_cond :: SpecWith (String, Int)
spec_queryWhat_no_cond = describe "no cond" $ do
  let makeWhat (t, w) = What { whatId = 0,
                               whatTitle = t,
                               whatWhen = w,
                               whatWheres = [],
                               whatBody = "",
                               whatTags = [],
                               whatCreatedAt = zeroTime,
                               whatUpdatedAt = zeroTime
                             }
      sampleOrder =
        map makeWhat
        [ ("01", Nothing),
          ("02", Just $ makeWhen 100 True  "+0000" ... makeWhen 200 True  "+0000"),
          ("03", Just $ makeWhen 110 True  "+0000" ... makeWhen 150 False "+0000"),
          ("04", Just $ makeWhen 100 False "+0000" ... makeWhen 300 True  "+0000"),
          ("05", Just $ makeWhen 100 True  "+0000" ... makeWhen 200 False "+0000"),
          ("06", Just $ makeWhen 100 False "+0000" ... makeWhen 300 False "+0000")
        ]
      q_asc = Query { queryCond = QCondTrue,
                      queryOrder = QOrderAsc,
                      queryOrderBy = QOrderByWhen,
                      queryRange = qRange 0 100
                    }
      exp_asc = ["06", "04", "05", "02", "03", "01"]
      q_desc = q_asc { queryOrder = QOrderDesc }
      exp_desc = reverse exp_asc
  specify "order by when" $ withCleanDB $ \conn -> do
    addWhats conn sampleOrder
    got_asc <- queryWhat conn q_asc
    (map whatTitle got_asc) `shouldBe` exp_asc
    got_desc <- queryWhat conn q_desc
    (map whatTitle got_desc) `shouldBe` exp_desc
  specify "order by when, range" $ withCleanDB $ \conn -> do
    addWhats conn sampleOrder
    let q_asc_r = q_asc { queryRange = qRange 2 5 }
        expSlice list indices = map (\i -> list !! i) indices
        q_desc_r = q_desc { queryRange = qRange 2 5 }
    got_asc <- queryWhat conn q_asc_r
    (map whatTitle got_asc) `shouldBe` (expSlice exp_asc [2 .. 4])
    got_desc <- queryWhat conn q_desc_r
    (map whatTitle got_desc) `shouldBe` (expSlice exp_desc [2 .. 4])


spec_queryWhat_tag :: SpecWith (String, Int)
spec_queryWhat_tag = describe "search for tags" $ do
  let makeWhat (title, tags) = What { whatId = 0,
                                      whatTitle = title,
                                      whatWhen = Nothing,
                                      whatWheres = [],
                                      whatBody = "",
                                      whatTags = tags,
                                      whatCreatedAt = zeroTime,
                                      whatUpdatedAt = zeroTime
                                    }
      sample =
        map makeWhat
        [ ("01", []),
          ("02", ["a"]),
          ("03", ["aa", "b"]),
          ("04", ["abc", "d"]),
          ("05", ["b", "a"])
        ]
      makeQuery cond = Query { queryCond = cond,
                               queryOrder = QOrderAsc,
                               queryOrderBy = QOrderByWhen,
                               queryRange = qRange 0 100
                             }
      specify' label cond expected =
        specify label $ withCleanDB $ \conn -> do
          addWhats conn sample
          got <- queryWhat conn $ makeQuery $ cond
          (map whatTitle got) `shouldMatchList` expected
  specify' "simple tag search"
    (QCondLeaf $ QCondTag "a")
    ["02", "05"]
  specify' "tag AND"
    (QCondAnd (QCondLeaf $ QCondTag "a") (QCondLeaf $ QCondTag "b"))
    ["05"]
  specify' "tag AND OR"
    ( QCondOr
      (QCondLeaf $ QCondTag "aa")
      (QCondAnd (QCondLeaf $ QCondTag "a") (QCondLeaf $ QCondTag "b"))
    )
    ["03", "05"]
  specify' "tag NOT"
    (QCondNot (QCondLeaf $ QCondTag "b"))
    ["01", "02", "04"]
  specify' "false"
    (QCondNot QCondTrue)
    []
  specify' "tag NOT AND"
    (QCondNot (QCondAnd (QCondLeaf $ QCondTag "a") (QCondLeaf $ QCondTag "b")))
    ["01", "02", "03", "04"]

spec_queryWhat_term :: SpecWith (String, Int)
spec_queryWhat_term = describe "search for term" $ do
  let makeWhat (title, tags, body) =
        What { whatId = 0,
               whatTitle = title,
               whatWhen = Nothing,
               whatWheres = [],
               whatBody = body,
               whatTags = tags,
               whatCreatedAt = zeroTime,
               whatUpdatedAt = zeroTime
             }
      sample =
        map makeWhat
        [ ("01 foo bar", ["aaa", "bbb"], "hoge fuga qwerty"),
          ("02 日本語のタイトル", ["bbb"], "quick brown fox jumped"),
          ("03 over the lazy dog", ["ccc"], "aaa 日本語のボディ")
        ]
      makeQuery cond = Query { queryCond = cond,
                               queryOrder = QOrderAsc,
                               queryOrderBy = QOrderByWhen,
                               queryRange = qRange 0 100
                             }
      specify' label cond expected =
        specify label $ withCleanDB $ \conn -> do
          addWhats conn sample
          got <- queryWhat conn $ makeQuery $ cond
          (map (T.take 2 . whatTitle) got) `shouldMatchList` expected
  specify' "body word"
    (QCondLeaf $ QCondTerm "fox")
    ["02"]
  specify' "tag and body"
    (QCondLeaf $ QCondTerm "aaa")
    ["01", "03"]
  specify' "title word OR tag"
    (QCondOr (QCondLeaf $ QCondTerm "foo") (QCondLeaf $ QCondTerm "ccc"))
    ["01", "03"]
  specify' "Japanese word"
    (QCondLeaf $ QCondTerm "日本語")
    ["02", "03"]
        
