-- |
-- Module: W5J.Time
-- Description: Time, DateTime and TimeZone implementation
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- This module is intended to hide the implementation specific to
-- 'TimeInstant' type.
module W5J.Time
       ( TimeInstant,
         TimeZone
       ) where

import Data.UnixTime (UnixTime)
import Data.Time.LocalTime (TimeZone)

-- | A time instant, without knowledge of any calendar systems or time
-- zones.
newtype TimeInstant = TimeInstant { unTimeInstant :: UnixTime }
                    deriving (Eq,Ord,Show)
