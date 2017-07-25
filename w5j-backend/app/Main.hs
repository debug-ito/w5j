{-# LANGUAGE OverloadedStrings #-}
module Main
       (main) where

import W5J.Time (zeroTime)
import W5J.What (What(..))
import W5J.DB.TinkerPop (Connection, withConnection, addWhat)

main :: IO ()
main = withConnection "localhost" 8182 mainWithConn

mainWithConn :: Connection -> IO ()
mainWithConn conn = print =<< addWhat conn what
  where
    what = What { whatId = 0,
                  whatTitle = "whaaat title",
                  whatTime = Nothing,
                  whatBody = "whaat body",
                  whatTags = ["foo", "bar", "buzz", "foo"],
                  whatCreatedAt = zeroTime,
                  whatUpdatedAt = zeroTime
                }


