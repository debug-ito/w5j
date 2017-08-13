-- |
-- Module: W5J.DB.TinkerPop.Query.Common
-- Description: common data structure for queries
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module W5J.DB.TinkerPop.Query.Common
       ( -- * Query
         Query(..),
         -- * QRange
         QRange(..),
         rangeMin,
         rangeMax,
         -- * QCondTree
         QCondTree(..),
         -- * QOrder
         QOrder(..)
       ) where

import W5J.Interval (Interval, sup, inf, (...))

-- | Range of elements indices to query.
newtype QRange = QRange { unQRange :: Interval Int }
               deriving (Eq,Show,Ord)

rangeMin :: QRange -> Int
rangeMin = inf . unQRange

rangeMax :: QRange -> Int
rangeMax = sup . unQRange


-- | condition tree. type @c@ is the leaf condition type.
--
-- TODO: how should we implement CondOr with gremlin??
data QCondTree c = QCondLeaf c
                 | QCondAnd (QCondTree c) (QCondTree c)
                 deriving (Show,Eq,Ord)

-- | Order asc/desc specifier.
data QOrder = QOrderAsc | QOrderDesc
            deriving (Show,Eq,Ord,Enum,Bounded)

-- | Generic query object. type @c@ is target-specific query condition
-- leaf. type @b@ is target-specific order base.
data Query c b =
  Query { queryCond :: QCondTree c,
          queryOrder :: QOrder,
          queryOrderBy :: b,
          queryRange :: QRange
        }
  deriving (Show,Eq,Ord)
