{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module: W5J.DB.TinkerPop.Query.Common
-- Description: common data structure for queries
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module W5J.DB.TinkerPop.Query.Common
       ( -- * Query
         Query(..),
         buildQueryWith,
         -- * QRange
         QRange,
         qRange,
         qRangeMin,
         qRangeMax,
         -- * QCondTree
         QCondTree(..),
         -- * QOrder
         QOrder(..),
         orderComparator
       ) where

import Data.Monoid ((<>))
import Control.Category ((>>>))

import W5J.Interval (Interval, sup, inf, (...))
import W5J.DB.TinkerPop.GBuilder (GBuilder, Gremlin, newPlaceHolder)
import W5J.DB.TinkerPop.GStep (GStep)
import qualified W5J.DB.TinkerPop.GStep as GStep

-- | Range of elements indices to query. Min is inclusive, max is
-- exclusive. Starting from 0.
newtype QRange = QRange { unQRange :: Interval Int }
               deriving (Eq,Show,Ord)

-- | Make 'QRange'.
qRange :: Int -> Int -> QRange
qRange a b = QRange (a ... b)

-- | Min of the range (inclusive.)
qRangeMin :: QRange -> Int
qRangeMin = inf . unQRange

-- | Max of the range (exclusive.)
qRangeMax :: QRange -> Int
qRangeMax = sup . unQRange


-- | condition tree. type @c@ is the leaf condition type, which is
-- specific to the data structure you are querying for.
data QCondTree c = QCondTrue
                 | QCondLeaf c
                 | QCondAnd (QCondTree c) (QCondTree c)
                 | QCondOr (QCondTree c) (QCondTree c)
                 | QCondNot (QCondTree c)
                 deriving (Show,Eq,Ord)

-- | Order asc/desc specifier.
data QOrder = QOrderAsc | QOrderDesc
            deriving (Show,Eq,Ord,Enum,Bounded)

-- | Predefined Gremlin comparator for QOrder
orderComparator :: QOrder -> Gremlin
orderComparator QOrderAsc = "incr"
orderComparator QOrderDesc = "decr"


-- | Generic query object. type @c@ is target-specific query condition
-- leaf. type @b@ is target-specific order base.
data Query c b =
  Query { queryCond :: QCondTree c,
          queryOrder :: QOrder,
          queryOrderBy :: b,
          queryRange :: QRange
        }
  deriving (Show,Eq,Ord)



-- TODO: 考えてみれば、inputとoutputが同じ型だからといってfilter系step
-- とは限らない。VertexとかEdgeとかの粒度だったら、traverseするのだっ
-- てinputとoutputが同じになりうる。GStepとGTraverseにもう一つ型パラメー
-- タ持たせて、filter系とそれ以外を区別するのがいいのでは。filter系
-- stepはインデックスにも絡むから重要だと思う。区別する意味はあるはず。



-- | Generate a filtering 'GStep' (i.e. a method call chain) from the
-- 'Query'.
buildQueryWith :: (c -> GBuilder (GStep GStep.Filter s s))
               -- ^ generate Gremlin step for the leaf condition @c@.
               -> (QOrder -> b -> GBuilder (GStep GStep.Filter s s))
               -- ^ generate Gremlin @.order@ and @.by@ step(s) for
               -- ordering.
               -> Query c b
               -> GBuilder (GStep GStep.Filter s s)
buildQueryWith buildCond buildOBy query = do
  gremlin_cond <- buildCondTree $ queryCond query
  gremlin_orderby <- buildOBy (queryOrder query) (queryOrderBy query)
  gremlin_range <- buildRange $ queryRange query
  return (gremlin_cond >>> gremlin_orderby >>> gremlin_range)
  where
    buildRange range = do
      v_min <- newPlaceHolder $ qRangeMin range
      v_max <- newPlaceHolder $ qRangeMax range
      return $ GStep.range v_min v_max
    buildCondTree (QCondOr a b) = do
      ga <- buildCondTree a
      gb <- buildCondTree b
      return $ GStep.or [ga, gb]
    buildCondTree (QCondAnd a b) = do
      ga <- buildCondTree a
      gb <- buildCondTree b
      return (ga >>> gb)
    buildCondTree (QCondNot a) = do
      ga <- buildCondTree a
      return $ GStep.not ga
    buildCondTree (QCondLeaf c) = buildCond c
    buildCondTree (QCondTrue) = return $ GStep.identity
