{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |
-- Module: W5J.DB.TinkerPop.GScript
-- Description: Low-level Gremlin script data type
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module W5J.DB.TinkerPop.GScript
       ( -- * Type
         GScript,
         -- * Constructors
         gRaw,
         gLiteral,
         -- * Conversions
         getGScript,
         -- * Placeholders
         PlaceHolderIndex,
         gPlaceHolder,
         toPlaceHolderVariable
       ) where

import Data.Monoid (Monoid(..))
import Data.String (IsString(..))
import Data.Text (Text, pack, unpack)
import qualified Data.Text.Lazy as TL

-- | Gremlin script data.
newtype GScript = GScript { unGScript :: TL.Text }
                deriving (Show,Eq,Ord,Monoid)

-- | Same as 'gLiteral' except for the input type.
instance IsString GScript where
  fromString = GScript . TL.pack . escapeDQuotes

escapeDQuotes :: String -> String
escapeDQuotes orig = f =<< orig
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
gRaw = GScript . TL.fromStrict

-- | Create a string literal in Gremlin script. The content is
-- automatically escaped.
gLiteral :: Text -> GScript
gLiteral = fromString . unpack

type PlaceHolderIndex = Int

-- | Create a placeholder variable with the given index.
gPlaceHolder :: PlaceHolderIndex -> GScript
gPlaceHolder = GScript . TL.fromStrict . toPlaceHolderVariable

-- | Create placeholder variable string from the index.
toPlaceHolderVariable :: PlaceHolderIndex -> Text
toPlaceHolderVariable i =  pack ("__v" ++ show i)

-- | Create a readable Gremlin script from 'GScript'.
getGScript :: GScript -> Text
getGScript = TL.toStrict . unGScript
