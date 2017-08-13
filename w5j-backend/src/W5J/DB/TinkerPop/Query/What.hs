{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module: W5J.DB.TinkerPop.Query.What
-- Description: queries for What
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module W5J.DB.TinkerPop.Query.What
       ( QueryWhat,
         buildQuery,
         QOrderBy(..),
         QCond(..)
       ) where

import Data.Monoid ((<>))
import Data.Text (Text)

import W5J.DB.TinkerPop.GBuilder (GBuilder, newPlaceHolder, Gremlin)
import W5J.DB.TinkerPop.Query.Common (Query, QOrder(..), buildQueryWith)

type QueryWhat = Query QCond QOrderBy

-- | order specifier for What data.
data QOrderBy = QOrderByWhen
              deriving (Show,Eq,Ord)

-- | condition specifier for What data
data QCond = QCondTerm Text
             -- ^ free term search entry for title, body, tags.
             deriving (Show,Eq,Ord)

buildQuery :: QueryWhat -> GBuilder Gremlin
buildQuery = buildQueryWith buildCond buildOrder
  where
    buildCond (QCondTerm t) = do
      vt <- newPlaceHolder t
      return (".or(__.has('title', textContains(" <> vt <> "))"
              <> ", __.has('body', textContains(" <> vt <> "))"
              <> ", __.has('tags', eq(" <> vt <> ")))")
    buildOrder order QOrderByWhen = undefined
      
