-- |
-- Module: W5J.DB.TinkerPop.Query.What
-- Description: queries for What
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module W5J.DB.TinkerPop.Query.What
       ( QueryWhat,
         QOrderBy(..),
         QCond(..)
       ) where

import Data.Text (Text)

import W5J.DB.TinkerPop.Query.Common (Query)

type QueryWhat = Query QCond QOrderBy

-- | order specifier for What data.
data QOrderBy = QOrderByWhen
              deriving (Show,Eq,Ord)

-- | condition specifier for What data
data QCond = QCondTerm Text
             -- ^ free term search entry for title, body, tags.
             deriving (Show,Eq,Ord)
