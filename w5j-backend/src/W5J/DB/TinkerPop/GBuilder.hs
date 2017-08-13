{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module: W5J.DB.TinkerPop.GBuilder
-- Description: GBuilder monad for building Gremlin
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module W5J.DB.TinkerPop.GBuilder
       ( GBuilder,
         PlaceHolderVariable,
         runGBuilder,
         newPlaceHolder
       ) where

import Control.Monad.Trans.State (State)
import qualified Control.Monad.Trans.State as State
import Data.Aeson (Value)
import Data.Monoid ((<>))
import qualified Data.HashMap.Strict as HM
import Data.Text (Text, pack)
import Database.TinkerPop.Types (Binding)


type PlaceHolderIndex = Int

type PlaceHolderVariable = Text

type GBuilder = State (PlaceHolderIndex, [Value])

newPlaceHolder :: Value -> GBuilder PlaceHolderVariable
newPlaceHolder val = do
  (next_index, values) <- State.get
  State.put (succ next_index, values ++ [val])
  return $ place next_index

place :: PlaceHolderIndex -> PlaceHolderVariable
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
