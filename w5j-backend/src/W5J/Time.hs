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
         currentTime,
         toEpochMsec,
         fromEpochMsec
       ) where

import Data.UnixTime
  ( UnixTime(..), getUnixTime,
    addUnixDiffTime, microSecondsToUnixDiffTime
  )
import Data.Time.LocalTime (TimeZone)
import Foreign.C.Types (CTime(..))

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

toEpochMsec :: TimeInstant -> Integer
toEpochMsec = toMsec . unTimeInstant
  where
    unCTime (CTime x) = x
    toMsec ut = ((toInteger $ unCTime $  utSeconds $ ut) * 1000)
                + ((toInteger $ utMicroSeconds $ ut) `div` 1000)

fromEpochMsec :: Integer -> TimeInstant
fromEpochMsec ems = TimeInstant
                    $ addUnixDiffTime (UnixTime 0 0) (microSecondsToUnixDiffTime (ems * 1000))
