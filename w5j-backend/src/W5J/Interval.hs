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
         sup
       ) where

import Numeric.Interval.NonEmpty
  ( Interval,
    (...),
    inf,
    sup
  )
