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
    allVertices, hasLabel, has, hasId, orderBy,
    unsafeGTraversal, values,
    GTraversal, Vertex, General
  )
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

buildQuery :: QueryWhat -> GBuilder (GTraversal General Void Vertex)
buildQuery query = do
  traversal <- buildQueryWith buildCond buildOrder query
  start <- makeStart
  return $ (start @. forgetFilter traversal)
  where
    makeStart = return (allVertices @. (forgetFilter $ hasLabel ["what"]))
    buildCond (QCondTerm t) = do
      vt <- newBind t
      -- For textContains predicate, see http://s3.thinkaurelius.com/docs/titan/1.0.0/index-parameters.html
      return $ GStep.or $
        [ has "title" (gFunCall "textContains" [vt]),
          has "body"  (gFunCall "textContains" [vt]),
          has "tags"  (gFunCall "eq" [vt])
        ]
    buildCond (QCondTag t) = do
      vt <- newBind t
      return $ has "tags" (gFunCall "eq" [vt])
    buildCond (QCondWhereID where_id) = do -- TODO: こいつのテストから。
      vid <- newBind where_id
      return $ GStep.filter $ filterTraversal vid
        where
          filterTraversal vid = GStep.out ["where"] >>> (forgetFilter $ hasId [vid])
    buildCond (QCondWhereName _) = undefined -- TODO
    buildCond (QCondWhen _ _ _) = undefined -- TODO
    buildOrder order QOrderByWhen =
      return $ orderBy [byWhen "when_from", byWhen "when_to", commonBy]
      where
        byWhen edge_label =
          (unsafeGTraversal (gFunCall "optionalT" [gFunCall "out" [gLiteral edge_label]]), comparator)
        comparator = case order of
          QOrderAsc -> gRaw "compareOptWhenVertices"
          QOrderDesc -> gRaw "compareOptWhenVertices.reversed()"
        commonBy =
          (toGTraversal $ void $ values ["updated_at"], orderComparator order)
      
