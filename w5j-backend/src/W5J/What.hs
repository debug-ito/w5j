-- |
-- Module: W5J.What
-- Description: data type and function about What node
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module W5J.What
       ( What(..),
         WhatID,
         Tag
       ) where

import Data.Text (Text)

import W5J.Interval (Interval)
import W5J.Time (TimeInstant)
import W5J.When (When)

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
