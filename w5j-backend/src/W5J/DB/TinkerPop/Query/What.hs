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

import Control.Category ((>>>))
import Data.Monoid ((<>))
import Data.Text (Text)

import W5J.DB.TinkerPop.GBuilder (GBuilder, newPlaceHolder, Gremlin)
import qualified W5J.DB.TinkerPop.GStep as GStep
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
buildQuery query = do
  traversal <- buildQueryWith buildCond buildOrder query
  start <- makeStart
  return $ GStep.toGremlin (start GStep.@. traversal)
  where
    makeStart = return (GStep.allVertices GStep.@. GStep.hasLabel "'what'")
    buildCond (QCondTerm t) = do
      vt <- newPlaceHolder t
      -- For textContains predicate, see http://s3.thinkaurelius.com/docs/titan/1.0.0/index-parameters.html
      return $ GStep.or $
        [ GStep.has "'title'" ("textContains(" <> vt <> ")"),
          GStep.has "'body'"  ("textContains(" <> vt <> ")"),
          GStep.has "'tags'"  ("eq(" <> vt <> ")")
        ]
    buildCond (QCondTag t) = do
      vt <- newPlaceHolder t
      return $ GStep.has "'tags'" ("eq(" <> vt <> ")")
    buildCond (QCondWhereID _) = undefined -- TODO
    buildCond (QCondWhereName _) = undefined -- TODO
    buildCond (QCondWhen _ _ _) = undefined -- TODO
    buildOrder order QOrderByWhen =
      return $ GStep.orderBy [byWhen "when_from", byWhen "when_to", commonBy]
      where
        byWhen edge_label =
          (GStep.unsafeFromGremlin ("optionalT(out('" <> edge_label <> "'))"), comparator)
        comparator = case order of
          QOrderAsc -> "compareOptWhenVertices"
          QOrderDesc -> "compareOptWhenVertices.reversed()"
        commonBy = -- ".by('updated_at', " <> orderComparator order <> ")"
          (GStep.unsafeFromGremlin "values('updated_at')", orderComparator order)
          -- TODO: make values GStep.
      
