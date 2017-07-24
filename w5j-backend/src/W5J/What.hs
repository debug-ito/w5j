-- |
-- Module: W5J.What
-- Description: data type and function about What vertex
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

-- | ID for 'What' vertex
type WhatID = Integer

-- | A short description of an 'What' vertex.
type Tag = Text

-- | 'What' vertex. It is a topic in the journal. For example, it can
-- be a planned event, a recipe, a book and an episode of an anime.
data What =
  What
  { whatId :: !WhatID,
    whatTitle :: !Text,
    whatTime :: !(Maybe (Interval When)),
    -- ^ optional time interval of the topic. For example, if the
    -- topic is an event, it is the interval during which the event is
    -- being held.
    whatBody :: !Text,
    whatTags :: ![Tag],
    whatCreatedAt :: !TimeInstant,
    whatUpdatedAt :: !TimeInstant
  }
  deriving (Eq,Ord,Show)
