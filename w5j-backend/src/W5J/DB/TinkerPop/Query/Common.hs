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
         QRange(..),
         rangeMin,
         rangeMax,
         -- * QCondTree
         QCondTree(..),
         -- * QOrder
         QOrder(..),
         orderComparator
       ) where

import Data.Monoid ((<>))

import W5J.Interval (Interval, sup, inf, (...))
import W5J.DB.TinkerPop.GBuilder (GBuilder, Gremlin, newPlaceHolder)

-- | Range of elements indices to query.
newtype QRange = QRange { unQRange :: Interval Int }
               deriving (Eq,Show,Ord)

rangeMin :: QRange -> Int
rangeMin = inf . unQRange

rangeMax :: QRange -> Int
rangeMax = sup . unQRange


-- | condition tree. type @c@ is the leaf condition type.
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

buildQueryWith :: (c -> GBuilder Gremlin)
               -> (QOrder -> b -> GBuilder Gremlin)
               -> Query c b
               -> GBuilder Gremlin
buildQueryWith buildCond buildOBy query = do
  gremlin_cond <- buildCondTree $ queryCond query
  gremlin_orderby <- buildOBy (queryOrder query) (queryOrderBy query)
  gremlin_range <- buildRange $ queryRange query
  return (gremlin_cond <> ".order()" <> gremlin_orderby <> gremlin_range)
  where
    buildRange range = do
      v_min <- newPlaceHolder $ rangeMin range
      v_max <- newPlaceHolder $ rangeMax range
      return (".range(" <> v_min <> ", " <> v_max <> ")")
    buildBinaryCond method a b = do
      ga <- buildCondTree a
      gb <- buildCondTree b
      return (". " <> method <> "(__" <> ga <> ", __" <> gb <> ")")
    buildCondTree (QCondAnd a b) = buildBinaryCond "and" a b
    buildCondTree (QCondOr a b) = buildBinaryCond "or" a b
    buildCondTree (QCondNot a) = do
      ga <- buildCondTree a
      return (".not(__" <> ga <> ")")
    buildCondTree (QCondLeaf c) = buildCond c
    buildCondTree (QCondTrue) = return ".identity()"
