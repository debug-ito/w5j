-- |
-- Module: W5J.Where
-- Description: data type and functions about Where vertex
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module W5J.Where
       ( Where(..),
         WhereID
       ) where

import Data.Text (Text)

type WhereID = Integer

data Where =
  Where
  { whereId :: !WhereID,
    whereName :: !Text
  }
  deriving (Show,Eq,Ord)
