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

import W5J.DB.TinkerPop.GBuilder (GBuilder, newPlaceHolder, Gremlin)
import W5J.DB.TinkerPop.GStep
  ( (@.), toGremlin, toGTraversal, forgetFilter,
    allVertices, hasLabel, has, hasId, orderBy,
    unsafeGTraversal, values
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

buildQuery :: QueryWhat -> GBuilder Gremlin
buildQuery query = do
  traversal <- buildQueryWith buildCond buildOrder query
  start <- makeStart
  return $ toGremlin (start @. forgetFilter traversal)
  where
    makeStart = return (allVertices @. (forgetFilter $ hasLabel ["'what'"]))
    buildCond (QCondTerm t) = do
      vt <- newPlaceHolder t
      -- For textContains predicate, see http://s3.thinkaurelius.com/docs/titan/1.0.0/index-parameters.html
      return $ GStep.or $
        [ has "'title'" ("textContains(" <> vt <> ")"),
          has "'body'"  ("textContains(" <> vt <> ")"),
          has "'tags'"  ("eq(" <> vt <> ")")
        ]
    buildCond (QCondTag t) = do
      vt <- newPlaceHolder t
      return $ has "'tags'" ("eq(" <> vt <> ")")
    buildCond (QCondWhereID where_id) = do
      vid <- newPlaceHolder where_id
      return $ GStep.filter $ filterTraversal vid
        where
          filterTraversal vid = GStep.out ["'where'"] >>> (forgetFilter $ hasId [vid])
    buildCond (QCondWhereName _) = undefined -- TODO
    buildCond (QCondWhen _ _ _) = undefined -- TODO
    buildOrder order QOrderByWhen =
      return $ orderBy [byWhen "when_from", byWhen "when_to", commonBy]
      where
        byWhen edge_label =
          (unsafeGTraversal ("optionalT(out('" <> edge_label <> "'))"), comparator)
        comparator = case order of
          QOrderAsc -> "compareOptWhenVertices"
          QOrderDesc -> "compareOptWhenVertices.reversed()"
        commonBy =
          (toGTraversal $ void $ values ["'updated_at'"], orderComparator order)
      
