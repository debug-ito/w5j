-- |
-- Module: W5J.When
-- Description: data type and function about When nodes
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module W5J.When
       ( When(..)
       ) where

import W5J.Time (TimeInstant, TimeZone)

data When =
  When
  { whenInstant :: !TimeInstant,
    whenIsTimeExplicit :: !Bool,
    whenTimeZone :: !TimeZone
  }
  deriving (Eq,Ord,Show)
