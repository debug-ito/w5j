-- |
-- Module: W5J.DB.TinkerPop.Query.Common
-- Description: common data structure for queries
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module W5J.DB.TinkerPop.Query.Common
       ( QRange,
         makeQRange,
         rangeMin,
         rangeMax
       ) where

import W5J.Interval (Interval, sup, inf, (...))

-- | Range of elements indices to query.
newtype QRange = QRange { unQRange :: Interval Int }
               deriving (Eq,Show,Ord)

makeQRange :: Int -> Int -> QRange
makeQRange a b = QRange (a ... b)

rangeMin :: QRange -> Int
rangeMin = inf . unQRange

rangeMax :: QRange -> Int
rangeMax = sup . unQRange

