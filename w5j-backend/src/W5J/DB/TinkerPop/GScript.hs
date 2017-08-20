-- |
-- Module: W5J.DB.TinkerPop.GScript
-- Description: Low-level Gremlin script data type
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module W5J.DB.TinkerPop.GScript
       () where

import Data.Monoid (Monoid(..))
import Data.String (IsString(..))
import Data.Text (Text, pack, unpack)


data GScriptElem = GSRaw !Text
                 | GSLiteral !Text
                 | GSPlaceHolder !Int
                 deriving (Show,Eq,Ord)

-- | Gremlin script data.
newtype GScript = GScript { unGScript :: ([GScriptElem] -> [GScriptElem]) }

toGScriptElems :: GScript -> [GScriptElem]
toGScriptElems gs = unGScript gs []

toTextDList :: GScript -> [Text] -> [Text]
toTextDList gs input = 

instance Show GScript where
  show = show . toGScriptElems

instance Eq GScript where
  a == b  = toGScriptElems a == toGScriptElems b

instance Monoid GScript where
  mempty = GScript id
  a `mappend` b = unGScript b . unGScript a

-- | Same as 'gLiteral' except for the input type.
instance IsString GScript where
  fromString s = (++ (GSLiteral $ pack $ escapeDQuotes s))

espaceDQuotes :: String -> String
espaceDQuotes orig = f =<< orig
  where
    f c = case c of
      '\n' -> "\\n"
      '\r' -> "\\r"
      '\t' -> "\\t"
      '\\' -> "\\\\"
      '"'  -> "\\\""
      x    -> [x]
      -- do we have to espace other characters?

-- | Create a raw Gremlin script. It is printed as-is.
gRaw :: Text -> GScript
gRaw t = (++ [GSRaw t])

-- | Create a string literal in Gremlin script. The content is
-- automatically escaped.
gLiteral :: Text -> GScript
gLiteral = fromString . unpack

type PlaceHolderIndex = Int

-- | Create a placeholder variable with the given index.
gPlaceHolder :: PlaceHolderIndex -> GScript
gPlaceHolder i = (++ [GPlaceHolder i])

-- | Create placeholder variable string from the index.
toPlaceHolderVariable :: PlaceHolderIndex -> Text
toPlaceHolderVariable i =  pack ("__v" ++ show i)

-- | Create a readable Gremlin script from 'GScript'.
getGScript :: GScript -> Text
getGScript = undefined
