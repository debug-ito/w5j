{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
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
import Control.Monad (void)
import Data.Aeson (toJSON)
import Data.Greskell
  ( Binder, GTraversal, Transform, Greskell, P, Walk,
    ($.), liftWalk,
    source, vertices,
    newBind,
    unsafeFunCall, toGremlin, unsafeGreskell, unsafeWalk,
    gOr, gHas2, gHas2P', pEq, gFilter, gOut, gHasId, gOrderBy, gHasLabel, gValues, gFold,
    ByComparator(..), pjTraversal, Order, ComparatorA, Comparator(cReversed),
    Element(..), Vertex, AVertexProperty,
    string,
    ComparatorA
  )
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Void (Void)

import W5J.DB.TinkerPop.Parse (AVertexWhat, AVertexWhere, AVertexWhen)
import W5J.DB.TinkerPop.Query.Common
  ( Query, QOrder(..),
    buildQueryWith, orderComparator
  )
import W5J.What (WhatID)
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
           | QCondWhenExists
             -- ^ match if 'whatWhen' is not 'Nothing'.
           | QCondWhen QCondWhenTerm QComparator When
             -- ^ compare 'whatWhen' with the given constant
             -- 'When'. This implies the 'whatWhen' is not 'Nothing'.
           deriving (Show,Eq,Ord)


buildQuery :: QueryWhat -> Binder (GTraversal Transform Void AVertexWhat)
buildQuery query = do
  traversal <- buildQueryWith buildCond buildOrder query
  return $ liftWalk traversal $. gHasLabel "what" $. vertices [] $ source "g"
  where
    -- For textContains predicate, see http://s3.thinkaurelius.com/docs/titan/1.0.0/index-parameters.html
    pTextContains :: Greskell Text -> Greskell (P Text)
    pTextContains t = unsafeFunCall "textContains" [toGremlin t]
    walkToWhere :: Walk Transform AVertexWhat AVertexWhere
    walkToWhere = gOut ["where"]
    buildCond (QCondTerm t) = do
      vt <- newBind t
      return $ gOr
        [ gHas2P' "title" (pTextContains vt),
          gHas2P' "body"  (pTextContains vt),
          gHas2P' "tags"  (pEq vt)
        ]
    buildCond (QCondTag t) = do
      vt <- newBind t
      return $ gHas2 "tags" vt
    buildCond (QCondWhereID where_id) = do -- TODO: こいつのテストから。いろいろあったけどようやく再開かな？ ていうか、まずgreskellをある程度モノにしよう。
      vid <- newBind where_id
      return $ gFilter (walkToWhere >>> gHasId vid)
    buildCond (QCondWhereName _) = undefined -- TODO
    buildCond (QCondWhenExists) = undefined -- TODO
    buildCond (QCondWhen _ _ _) = undefined -- TODO
    buildOrder order QOrderByWhen =
      return $ gOrderBy [byWhen "when_from", byWhen "when_to", commonBy]
      where
        byWhen edge_label = ByComp (pjTraversal $ getOptWhen edge_label) comparator
        getOptWhen edge_label = gOut [edge_label] >>> gFold >>> listToOptionalWalk
        comparator :: Greskell (ComparatorA (Maybe AVertexWhen))
        comparator = case order of
          QOrderAsc -> compareOptWhenVertices
          QOrderDesc -> cReversed compareOptWhenVertices
        compareOptWhenVertices :: Greskell (ComparatorA (Maybe AVertexWhen))
        compareOptWhenVertices = unsafeGreskell "compareOptWhenVertices"
        commonBy =
          ByComp (pjTraversal $ gValues ["updated_at"]) (orderComparator order)
        listToOptionalWalk :: Walk Transform [a] (Maybe a)
        listToOptionalWalk = unsafeWalk "map" ["{ listToOptional(it.get()) }"]
      
