-- |
-- Module: W5J.DB.TinkerPop.Query.What
-- Description: queries for What
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module W5J.DB.TinkerPop.Query.What
       ( QOrder(..)
       ) where

-- | order specifier for What data.
data QOrder = QOrderByWhen
            deriving (Show,Eq,Ord)

