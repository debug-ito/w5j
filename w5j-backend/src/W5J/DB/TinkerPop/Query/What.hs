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
import Control.Monad (void)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Void (Void)

import W5J.DB.TinkerPop.GBuilder (GBuilder, newBind, GScript)
import W5J.DB.TinkerPop.GScript (gRaw, gFunCall, gLiteral)
import W5J.DB.TinkerPop.GStep
  ( (@.), toGScript, toGTraversal, forgetFilter,
    allVertices, gHasLabel, gHas, gHasId, gOrderBy,
    unsafeGTraversal, gValues, gFilter, gOut, gOr,
    GTraversal, Vertex, Transform
  )
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

buildQuery :: QueryWhat -> GBuilder (GTraversal Transform Void Vertex)
buildQuery query = do
  traversal <- buildQueryWith buildCond buildOrder query
  start <- makeStart
  return $ (start @. forgetFilter traversal)
  where
    makeStart = return (allVertices @. (forgetFilter $ gHasLabel ["what"]))
    buildCond (QCondTerm t) = do
      vt <- newBind t
      -- For textContains predicate, see http://s3.thinkaurelius.com/docs/titan/1.0.0/index-parameters.html
      return $ gOr $
        [ gHas "title" (gFunCall "textContains" [vt]),
          gHas "body"  (gFunCall "textContains" [vt]),
          gHas "tags"  (gFunCall "eq" [vt])
        ]
    buildCond (QCondTag t) = do
      vt <- newBind t
      return $ gHas "tags" (gFunCall "eq" [vt])
    buildCond (QCondWhereID where_id) = do -- TODO: こいつのテストから。
      vid <- newBind where_id
      return $ gFilter $ filterTraversal vid
        where
          filterTraversal vid = gOut ["where"] >>> (forgetFilter $ gHasId [vid])
    buildCond (QCondWhereName _) = undefined -- TODO
    buildCond (QCondWhen _ _ _) = undefined -- TODO
    buildOrder order QOrderByWhen =
      return $ gOrderBy [byWhen "when_from", byWhen "when_to", commonBy]
      where
        byWhen edge_label =
          (unsafeGTraversal (gFunCall "optionalT" [gFunCall "out" [gLiteral edge_label]]), comparator)
        comparator = case order of
          QOrderAsc -> gRaw "compareOptWhenVertices"
          QOrderDesc -> gRaw "compareOptWhenVertices.reversed()"
        commonBy =
          (toGTraversal $ void $ gValues ["updated_at"], orderComparator order)
      
