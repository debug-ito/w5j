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
import W5J.DB.TinkerPop.Query.Common
  ( Query, QOrder(..),
    buildQueryWith, orderComparator
  )
import W5J.When (When)
import W5J.Where (WhereID)

type QueryWhat = Query QCond QOrderBy

-- | order specifier for What data.
data QOrderBy = QOrderByWhen
              deriving (Show,Eq,Ord)

-- | comparator symbols for conditions. (this may be in
-- "W5J.DB.TinkerPop.Query.Common" module.)
data QComparator = QCompLte
                 | QCompGte
                 deriving (Show,Eq,Ord)

-- | condition symbols about 'whatWhen'.
data QCondWhenTerm = QCondWhenFrom
                   | QCondWhenTo
                   deriving (Show,Eq,Ord)

-- | condition specifier for What data
data QCond = QCondTerm Text
             -- ^ free term search entry for title, body, tags.
           | QCondTag Text
             -- ^ exact match for tags.
           | QCondWhereID WhereID
             -- ^ match for 'whatWheres' by ID.
           | QCondWhereName Text
             -- ^ match for 'whatWheres' by name.
           | QCondWhen QCondWhenTerm QComparator When
             -- ^ compare 'whatWhen' with the given constant 'When'.
           deriving (Show,Eq,Ord)

buildQuery :: QueryWhat -> GBuilder Gremlin
buildQuery = buildQueryWith buildCond buildOrder
  where
    buildCond (QCondTerm t) = do
      vt <- newPlaceHolder t
      -- For textContains predicate, see http://s3.thinkaurelius.com/docs/titan/1.0.0/index-parameters.html
      return (".or(__.has('title', textContains(" <> vt <> "))"
              <> ", __.has('body', textContains(" <> vt <> "))"
              <> ", __.has('tags', eq(" <> vt <> ")))")
    buildCond (QCondTag t) = do
      vt <- newPlaceHolder t
      return (".has('tags', eq(" <> vt <> "))")
    buildCond (QCondWhereID _) = undefined -- TODO
    buildCond (QCondWhereName _) = undefined -- TODO
    buildCond (QCondWhen _ _ _) = undefined -- TODO
    buildOrder order QOrderByWhen =
      return (byStep "when_from" <> byStep "when_to" <> commonBy)
      where
        byStep edge_label = ".by(optionalT(out('" <> edge_label <> "')), " <> comparator <> ")"
        comparator = case order of
          QOrderAsc -> "compareOptWhenVertices"
          QOrderDesc -> "compareOptWhenVertices.reversed()"
        commonBy = ".by('updated_at', " <> orderComparator order <> ")"
      
