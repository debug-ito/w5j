{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module: W5J.GremlinTest
-- Description: 
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module W5J.GremlinTest
       ( main
       ) where

import qualified Database.TinkerPop as TP
import Database.TinkerPop.Types (Connection)

main :: IO ()
main = TP.run "localhost" 8182 doTest

doTest :: Connection -> IO ()
doTest conn = print =<< TP.submit conn "g.E()" Nothing

-- --- For "g.V()"
-- 
-- Right [
--   Object (fromList [
--     ("id",Number 4128.0),
--     ("type",String "vertex"),
--     ("label",String "what"),
--     ("properties",Object (fromList [
--       ("title",Array [
--         Object (fromList [("value",String "hoge hoge hoge"),("id",String "16s-36o-1l1")])
--       ])
--     ]))
--   ]),
--   Object (fromList [
--     ("id",Number 4192.0),
--     ("type",String "vertex"),
--     ("label",String "when"),
--     ("properties",Object (fromList [
--       ("instant_ms",Array [
--         Object (fromList [("value",Number 1.1929121e7),("id",String "170-38g-35x")])
--       ]),
--       ("is_time_explicit",Array [
--         Object (fromList [("value",Bool True),("id",String "1l8-38g-3yd")])
--       ]),
--       ("time_zone",Array [
--         Object (fromList [("value",String "+0900"),("id",String "1zg-38g-4qt")])]
--       )])
--     )
--   ])
-- ]



-- --- For g.E()
-- 
-- Right [
--   Object (fromList [
--     ("outV",Number 4128.0),
--     ("inV",Number 4192.0),
--     ("inVLabel",String "when"),
--     ("id",String "1l0-36o-5jp-38g"),
--     ("type",String "edge"),
--     ("outVLabel",String "what"),
--     ("label",String "when_from"),
--     ("properties",Object (fromList [
--       ("created_at",Number 1.01029102e8)]))
--     ]
--   )
-- ]
