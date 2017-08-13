{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module: W5J.DB.TinkerPop.GBuilder
-- Description: GBuilder monad for building Gremlin
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module W5J.DB.TinkerPop.GBuilder
       ( -- * Types
         GBuilder,
         Gremlin,
         Binding,
         PlaceHolderVariable,
         -- * Actions
         newPlaceHolder,
         -- * Runners
         runGBuilder,
         submitGBuilder
       ) where

import Control.Monad.Trans.State (State)
import qualified Control.Monad.Trans.State as State
import Data.Aeson (Value, ToJSON(toJSON))
import Data.Monoid ((<>))
import qualified Data.HashMap.Strict as HM
import Data.Text (Text, pack)
import Database.TinkerPop.Types (Binding, Connection, Gremlin)
import qualified Database.TinkerPop as TP

type PlaceHolderIndex = Int

type PlaceHolderVariable = Text

type GBuilder = State (PlaceHolderIndex, [Value])

newPlaceHolder :: ToJSON v => v -> GBuilder PlaceHolderVariable
newPlaceHolder val = do
  (next_index, values) <- State.get
  State.put (succ next_index, values ++ [toJSON val])
  return $ place next_index

place :: PlaceHolderIndex -> PlaceHolderVariable
place index = "__v" <> (pack $ show index)

runGBuilder :: GBuilder a -> (a, Binding)
runGBuilder gbuilder = (ret, binding)
  where
    (ret, (_, values)) = State.runState gbuilder (0, [])
    binding = HM.fromList $ zip (map place [0 ..]) $ values

submitGBuilder :: Connection -> GBuilder Gremlin -> IO (Either String [Value])
submitGBuilder conn gbuilder = TP.submit conn gremlin (Just binding)
  where
    (gremlin, binding) = runGBuilder gbuilder

-- seqGremlin :: [GBuilder Text] -> GBuilder Text
-- seqGremlin = fmap seqSentences . sequence
--   where
--     seqSentences = T.intercalate "; "
-- 
