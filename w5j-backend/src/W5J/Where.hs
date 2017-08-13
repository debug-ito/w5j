-- |
-- Module: W5J.Where
-- Description: data type and functions about Where vertex
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module W5J.Where
       ( Where(..),
         WhereID,
         whereFromName
       ) where

import Data.Text (Text)

type WhereID = Integer

data Where =
  Where
  { whereId :: !(Maybe WhereID),
    -- ^ ID is optional.
    whereName :: !Text
    -- ^ name of Where vertex is unique.
  }
  deriving (Show,Eq,Ord)

whereFromName :: Text -> Where
whereFromName n = Where { whereId = Nothing,
                          whereName = n
                        }
