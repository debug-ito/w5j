{-# LANGUAGE OverloadedStrings #-}
module Main
       (main) where

import W5J.Interval ((...))
import W5J.Time (zeroTime)
import W5J.What (What(..))
import W5J.When (currentUTCWhen)
import W5J.DB.TinkerPop (Connection, withConnection, addWhat)

main :: IO ()
main = withConnection "localhost" 8182 mainWithConn

mainWithConn :: Connection -> IO ()
mainWithConn conn = do
  cur_when <- currentUTCWhen
  print =<< addWhat conn (what cur_when)
  where
    what cur_when = What { whatId = 0,
                           whatTitle = "whaaat title",
                           whatTime = Just (cur_when ... cur_when),
                           whatBody = "whaat body",
                           whatTags = ["foo", "bar", "buzz", "foo"],
                           whatCreatedAt = zeroTime,
                           whatUpdatedAt = zeroTime
                         }


