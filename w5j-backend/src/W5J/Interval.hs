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

import qualified Numeric.Interval.NonEmpty as NI

newtype Interval a = Interval { unNI :: NI.Interval a }
                     deriving (Show)

-- | Structural 'Eq' instance. This is the behavior of as of
-- intervals-0.8. Before 0.8, the 'Eq' instance was defined
-- differently.
instance Eq a => Eq (Interval a) where
  a == b = (inf a == inf b) && (sup a == sup b)

-- | Same as 'Eq' instance.
instance Ord a => Ord (Interval a) where
  compare a b = let inf_ret = compare (inf a) (inf b)
                in if inf_ret == EQ
                   then compare (sup a) (sup b)
                   else inf_ret

(...) :: Ord a => a -> a -> Interval a
a ... b = Interval ((NI....) a b)

inf :: Interval a -> a
inf = NI.inf . unNI

sup :: Interval a -> a 
sup = NI.sup . unNI

mapInterval :: Ord b => (a -> b) -> Interval a -> Interval b
mapInterval f aint = binf ... bsup
  where binf = f $ inf aint
        bsup = f $ sup aint
