-- |
-- Module: W5J.Time
-- Description: Time, DateTime and TimeZone implementation
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- This module is intended to hide the implementation specific to
-- 'TimeInstant' type.
module W5J.Time
       ( TimeInstant,
         TimeZone,
         zeroTime,
         currentTime
       ) where

import Data.UnixTime (UnixTime(..), getUnixTime)
import Data.Time.LocalTime (TimeZone)

-- | A time instant, without knowledge of any calendar systems or time
-- zones.
newtype TimeInstant = TimeInstant { unTimeInstant :: UnixTime }
                    deriving (Eq,Ord,Show)

-- | The time at \"zero\" in the timeline. Use this just for a
-- placeholder.
zeroTime :: TimeInstant
zeroTime = TimeInstant $ UnixTime 0 0

-- | Get the current time from the system.
currentTime :: IO TimeInstant
currentTime = fmap TimeInstant $ getUnixTime
