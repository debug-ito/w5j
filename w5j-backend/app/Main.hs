{-# LANGUAGE OverloadedStrings #-}
module Main
       (main) where

import W5J.Interval ((...))
import W5J.Time (zeroTime)
import W5J.What (What(..))
import W5J.When (currentUTCWhen)
import W5J.Where (Where(..))
import W5J.DB.TinkerPop (Connection, withConnection, addWhat, addWhat', getWhatById)

main :: IO ()
main = withConnection "localhost" 8182 mainWithConn

mainWithConn :: Connection -> IO ()
mainWithConn conn = do
  cur_when <- currentUTCWhen
  wid <- addWhat' conn (what cur_when)
  putStrLn ("WhatID = " ++ show wid)
  print =<< getWhatById conn wid
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


