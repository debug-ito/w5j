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
         GScript,
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

import W5J.DB.TinkerPop.GScript
  ( GScript, gPlaceHolder, toPlaceHolderVariable,
    PlaceHolderIndex, getGScript
  )
import W5J.DB.TinkerPop.IO.Connection (Binding, Connection, submit)


newtype GBuilder a = GBuilder { unGBuilder :: State (PlaceHolderIndex, [Value]) a }
                   deriving (Functor, Applicative, Monad)

newPlaceHolder :: ToJSON v => v -> GBuilder GScript
newPlaceHolder val = GBuilder $ do
  (next_index, values) <- State.get
  State.put (succ next_index, values ++ [toJSON val])
  return $ gPlaceHolder next_index

runGBuilder :: GBuilder a -> (a, Binding)
runGBuilder gbuilder = (ret, binding)
  where
    (ret, (_, values)) = State.runState (unGBuilder gbuilder) (0, [])
    binding = HM.fromList $ zip (map toPlaceHolderVariable [0 ..]) $ values

submitGBuilder :: Connection -> GBuilder GScript -> IO (Either String [Value])
submitGBuilder conn gbuilder = submit conn gremlin mbinding
  where
    (gscript, binding_map) = runGBuilder gbuilder
    gremlin = getGScript gscript
    mbinding = if HM.null binding_map
               then Nothing
               else Just binding_map

-- seqGremlin :: [GBuilder Text] -> GBuilder Text
-- seqGremlin = fmap seqSentences . sequence
--   where
--     seqSentences = T.intercalate "; "
-- 
