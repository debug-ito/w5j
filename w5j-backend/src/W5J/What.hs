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
    whatTags :: ![Tag], -- TODO: should we use Set?
    whatCreatedAt :: !TimeInstant,
    whatUpdatedAt :: !TimeInstant
  }
  deriving (Eq,Ord,Show)

-- TODO: そういやWhereリストも必要。ていうか、When (vertex)とWhere
-- (vertex)をWhatの中に入れ込む作りにすると、DBでのWhatのupdate処理が
-- めちゃくちゃ面倒になりそうな気がする。抽象化のレベルは下がるが、
-- WhatとWhen, Whereは切り離して管理したほうがDB側の実装はシンプルにな
-- るし、DBのユーザ側のコードもそんなに困らない気がする。どのみちHowは
-- 切り離しているし(Howは数が多くなるかもしれないので切り離すべき)

-- なんならWhat, When, Whereを全て抱き込んだデータ型を作ってもよい。で
-- もそれはDBのI/Fにはならない。

-- ・・・と思ったけど、やはりWhenはWhatと同じのトランザクションで処理
-- したほうがいい。Whereはどうする？Whereもvertexにしたいけど、Whenと
-- 違って気軽に消したりできない。うーん。
