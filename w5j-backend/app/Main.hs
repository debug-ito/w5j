{-# LANGUAGE OverloadedStrings #-}
module Main
       (main) where

import W5J.Interval ((...))
import W5J.Time (zeroTime)
import W5J.What (What(..))
import W5J.When (currentUTCWhen)
import W5J.DB.TinkerPop (Connection, withConnection, addWhat, getWhatById)

main :: IO ()
main = withConnection "localhost" 8182 mainWithConn

mainWithConn :: Connection -> IO ()
mainWithConn conn = do
  cur_when <- currentUTCWhen
  wid <- addWhat conn (what cur_when)
  putStrLn ("WhatID = " ++ show wid)
  print =<< getWhatById conn wid
  where
    what cur_when = What { whatId = 0,
                           whatTitle = "whaaat title",
                           -- whatTime = Just (cur_when ... cur_when),
                           whatTime = Nothing,
                           whatBody = "whaat body",
                           whatTags = ["foo", "bar", "buzz", "foo"],
                           whatCreatedAt = zeroTime,
                           whatUpdatedAt = zeroTime
                         }


