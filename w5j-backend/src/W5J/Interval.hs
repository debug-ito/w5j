-- |
-- Module: W5J.Interval
-- Description: Interval type encapsulation
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- This module is intended to hide the implementation specific to
-- 'Interval' type.
module W5J.Interval
       ( Interval,
         (...),
         inf,
         sup,
         mapInterval
       ) where

import Numeric.Interval.NonEmpty
  ( Interval,
    (...),
    inf,
    sup
  )

mapInterval :: Ord b => (a -> b) -> Interval a -> Interval b
mapInterval f aint = binf ... bsup
  where binf = f $ inf aint
        bsup = f $ sup aint
