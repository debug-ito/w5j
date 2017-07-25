-- |
-- Module: W5J.When
-- Description: data type and function about When vertex
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module W5J.When
       ( When(..),
         currentUTCWhen
       ) where

import Control.Applicative ((<$>), (<*>), pure)
import W5J.Time (TimeInstant, TimeZone, utcTimeZone, currentTime)

-- | 'When' vertex. It is a date or datetime. In either case, it has
-- 'TimeZone'.
data When =
  When
  { whenInstant :: !TimeInstant,
    whenIsTimeExplicit :: !Bool,
    -- ^ If 'True', the time part in 'whenInstant' is explicit and
    -- should be presented to the user. If 'False', the time part
    -- should be ignored.
    whenTimeZone :: !TimeZone
  }
  deriving (Eq,Ord,Show) -- TODO: Ord should not consider TimeZone

currentUTCWhen :: IO When
currentUTCWhen = When <$> currentTime <*> pure True <*> pure utcTimeZone
