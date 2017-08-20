{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
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

import W5J.DB.TinkerPop.IO.Connection (Binding, Connection, Gremlin, submit)

type PlaceHolderIndex = Int

type PlaceHolderVariable = Text

newtype GBuilder a = GBuilder { unGBuilder :: State (PlaceHolderIndex, [Value]) a }
                   deriving (Functor, Applicative, Monad)

newPlaceHolder :: ToJSON v => v -> GBuilder PlaceHolderVariable
newPlaceHolder val = GBuilder $ do
  (next_index, values) <- State.get
  State.put (succ next_index, values ++ [toJSON val])
  return $ place next_index

place :: PlaceHolderIndex -> PlaceHolderVariable
place index = "__v" <> (pack $ show index)

runGBuilder :: GBuilder a -> (a, Binding)
runGBuilder gbuilder = (ret, binding)
  where
    (ret, (_, values)) = State.runState (unGBuilder gbuilder) (0, [])
    binding = HM.fromList $ zip (map place [0 ..]) $ values

submitGBuilder :: Connection -> GBuilder Gremlin -> IO (Either String [Value])
submitGBuilder conn gbuilder = submit conn gremlin mbinding
  where
    (gremlin, binding_map) = runGBuilder gbuilder
    mbinding = if HM.null binding_map
               then Nothing
               else Just binding_map

-- seqGremlin :: [GBuilder Text] -> GBuilder Text
-- seqGremlin = fmap seqSentences . sequence
--   where
--     seqSentences = T.intercalate "; "
-- 
