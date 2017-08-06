{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module: W5J.DB.TinkerPop
-- Description: DB backend for TinkerPop Gremlin Server
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module W5J.DB.TinkerPop
       ( Connection,
         withConnection,
         addWhat,
         updateWhat,
         getWhatById,
         deleteWhat
       ) where

import Control.Monad.Trans.State (State)
import qualified Control.Monad.Trans.State as State
import Data.Aeson (ToJSON(toJSON), Value)
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HM
import Data.Maybe (listToMaybe)
import Data.Monoid ((<>), mconcat)
import Data.Text (pack, Text)
import qualified Data.Text as T
import Database.TinkerPop.Types
  ( Connection, Binding
  )
import qualified Database.TinkerPop as TP

import W5J.DB.TinkerPop.Error (toGremlinError, parseError)
import W5J.DB.TinkerPop.Parse (ioFromJSON, unACompleteWhat)
import W5J.Aeson (toAWhat)
import W5J.Interval (inf, sup)
import W5J.Time (currentTime, toEpochMsec)
import W5J.What (What(..), WhatID)
import W5J.When (When(..))


-- | Make a 'Connection' to the given server and run the given action.
withConnection :: String
               -- ^ server hostname
               -> Int
               -- ^ server port number
               -> (Connection -> IO ())
               -- ^ action
               -> IO ()
withConnection = TP.run


-- We (sometimes) use multiple Gremlin sentences in a single
-- request. This is because by default each request is enclosed in a
-- transaction. TinkerPop allows transactions over multiple requests
-- by means of "session", but gremlin-haskell (as of 0.1.0.2) does not
-- support it.
--
-- See:
--
-- - https://groups.google.com/forum/#!topic/gremlin-users/S4T9yOfHlV4
-- - http://tinkerpop.apache.org/docs/3.0.1-incubating/#sessions

-- | Add a new 'What' vertex into the DB.
addWhat :: Connection
        -> What
        -- ^ 'What' vertex to add. 'whatId', 'whatCreatedAt' and
        -- 'whatUpdatedAt' fields are ignored, and set automatically.
        -> IO (WhatID)
addWhat conn what = do
  cur_time <- currentTime
  let (gremlin, binds) = getGremlin $ setCurrentTime cur_time what
  parseResult =<< toGremlinError =<< TP.submit conn gremlin (Just binds)
  where
    setCurrentTime t w = w { whatCreatedAt = t,
                             whatUpdatedAt = t
                           }
    getGremlin w = runGBuilder $ do
      p <- fmap place $ newPlaceHolder $ Aeson.toJSON $ toAWhat w
      return ("addWhat(" <> p <> ").id()")
    parseResult [] = parseError "No element in the result."
    parseResult (ret : _) = ioFromJSON ret
    
-- | Update an existing 'What' vertex specified by the 'whatId' field.
updateWhat :: Connection
           -> What
           -- ^ 'whatUpdatedAt' field is ignored and set
           -- automatically.
           -> IO ()
updateWhat = undefined
-- TODO. We can just delete and re-create When vertices.

-- | Get 'What' vertex with the given 'WhatID'.
getWhatById :: Connection -> WhatID -> IO (Maybe What)
getWhatById conn wid = do
  mgot_val <- fmap (listToMaybe) $ toGremlinError =<< TP.submit conn gremlin (Just binds)
  case mgot_val of
   Nothing -> return Nothing
   Just got_val -> fmap (Just . unACompleteWhat) $ ioFromJSON got_val
  where
    gremlin = "g.V(WID).hasLabel('what')"
              <> ".map({ v = it.get(); [v, __(v).out('when_from').toList(), __(v).out('when_to').toList(), "
              <>  "__(v).out('where').toList()]})"
    binds = HM.fromList [("WID", toJSON wid)]

-- vertex propertyのvalueは必ずArrayに入っている。これでSETやLIST cardinalityを表現している。
-- when_fromやwhen_toエッジがない場合は後ろの2つのリストが空になる。なるほど。
-- bottom upでFromJSONを書いていくといいのでは。

--- [Array [
---   Object (fromList [
---     ("id",Number 4112.0),
---     ("type",String "vertex"),
---     ("label",String "what"),
---     ("properties",Object (fromList [
---         ("body",Array [Object (fromList [("value",String "whaat body"),("id",String "1ky-368-35x")])]),
---         ("updated_at",Array [Object (fromList [("value",Number 1.500972323489e12),("id",String "2de-368-4qt")])]),
---         ("created_at",Array [Object (fromList [("value",Number 1.500972323489e12),("id",String "1z6-368-3yd")])]),
---         ("title",Array [Object (fromList [("value",String "whaaat title"),("id",String "16q-368-2dh")])]),
---         ("tags",Array [
---             Object (fromList [("value",String "bar"),("id",String "35u-368-sl")]),
---             Object (fromList [("value",String "buzz"),("id",String "3k2-368-sl")]),
---             Object (fromList [("value",String "foo"),("id",String "3ya-368-sl")])])]))]),
---   Array [
---     Object (fromList [
---      ("id",Number 4152.0),
---      ("type",String "vertex"),
---      ("label",String "when"),
---      ("properties",Object (fromList [
---         ("instant",Array [Object (fromList [("value",Number 1.500972323489e12),("id",String "16v-37c-6bp")])]),
---         ("is_time_explicit",Array [Object (fromList [("value",Bool True),("id",String "1l3-37c-745")])]),
---         ("time_zone",Array [Object (fromList [("value",String "DUMMY_TZ"),("id",String "1zb-37c-7wl")])])]))])],
---    Array [
---      Object (fromList [
---        ("id",Number 4336.0),
---        ("type",String "vertex"),
---        ("label",String "when"),
---        ("properties",Object (fromList [
---           ("instant",Array [Object (fromList [("value",Number 1.500972323489e12),("id",String "17i-3cg-6bp")])]),
---           ("is_time_explicit",Array [Object (fromList [("value",Bool True),("id",String "1lq-3cg-745")])]),
---           ("time_zone",Array [Object (fromList [("value",String "DUMMY_TZ"),("id",String "1zy-3cg-7wl")])])]))])]]
--- ]

-- | Delete a 'What' vertex.
deleteWhat :: Connection -> WhatID -> IO ()
deleteWhat = undefined
-- TODO. how should we treat other vertices connected (directly or
-- non-directly) connected to the deleted vertex?


type PlaceHolderIndex = Int

type GBuilder = State (PlaceHolderIndex, [Value])

newPlaceHolder :: Value -> GBuilder PlaceHolderIndex
newPlaceHolder val = do
  (next_index, values) <- State.get
  State.put (succ next_index, values ++ [val])
  return next_index

place :: PlaceHolderIndex -> Text
place index = "v" <> (pack $ show index)

runGBuilder :: GBuilder a -> (a, Binding)
runGBuilder gbuilder = (ret, binding)
  where
    (ret, (_, values)) = State.runState gbuilder (0, [])
    binding = HM.fromList $ zip (map place [0 ..]) $ values

-- seqGremlin :: [GBuilder Text] -> GBuilder Text
-- seqGremlin = fmap seqSentences . sequence
--   where
--     seqSentences = T.intercalate "; "
-- 
