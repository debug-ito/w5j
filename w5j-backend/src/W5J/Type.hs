-- |
-- Module: W5J.Type
-- Description: data types used in W5J
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module W5J.Type
       ( What(..),
         WhatID,
         Tag,
         When(..)
       ) where

import Data.Text (Text)

import W5J.Interval (Interval)
import W5J.Time (TimeInstant, TimeZone)

type WhatID = Integer

type Tag = Text

data What =
  What
  { whatId :: !WhatID,
    whatTitle :: !Text,
    whatTime :: !(Maybe (Interval When)),
    whatBody :: !Text,
    whatTags :: ![Tag],
    whatCreatedAt :: !TimeInstant,
    whatUpdatedAt :: !TimeInstant
  }
  deriving (Eq,Ord,Show)

data When =
  When
  { whenInstant :: !TimeInstant,
    whenIsTimeExplicit :: !Bool,
    whenTimeZone :: !TimeZone
  }
  deriving (Eq,Ord,Show)

