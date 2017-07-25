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
                  whatTitle = "what title",
                  whatTime = Nothing,
                  whatBody = "what body",
                  whatTags = ["foo", "bar", "buzz"],
                  whatCreatedAt = zeroTime,
                  whatUpdatedAt = zeroTime
                }


