{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module: W5J.DB.TinkerPop.Parse
-- Description: parsers for responses from Gremlin Server
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module W5J.DB.TinkerPop.Parse
       ( PropertyValue(..),
         PropertyOne(..),
         PropertyMany(..),
         VertexID,
         VertexLabel,
         parseVertex
       ) where

import Control.Applicative ((<$>), empty)
import Control.Monad (mapM, guard)
import Data.Aeson (FromJSON(parseJSON), Object, (.:), Value(Object,Array))
import Data.Aeson.Types (Parser)
import Data.Foldable (toList)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)


-- | The atomic (vertex) property value
newtype PropertyValue a = PropertyValue { unPropertyValue :: a }
                        deriving (Show,Eq,Ord)

instance FromJSON a => FromJSON (PropertyValue a) where
  parseJSON (Object obj) = PropertyValue <$> (obj .: "value")
  parseJSON _ = empty

-- | Property value of ONE cardinality
newtype PropertyOne a = PropertyOne { unPropertyOne :: PropertyValue a }
                      deriving (Show,Eq,Ord)

instance FromJSON a => FromJSON (PropertyOne a) where
  parseJSON v = do
    values <- fmap unPropertyMany $ parseJSON v
    case values of
     [head_v] -> return $ PropertyOne head_v
     _ -> empty

-- | Property value of LIST/SET cardinality
newtype PropertyMany a = PropertyMany { unPropertyMany :: [PropertyValue a] }
                       deriving (Show,Eq,Ord)

instance FromJSON a => FromJSON (PropertyMany a) where
  parseJSON (Array arr) = fmap (PropertyMany . map PropertyValue) $ mapM parseJSON $ toList arr
  parseJSON _ = empty

type VertexID = Integer
type VertexLabel = Text

parseVertex :: (VertexID -> VertexLabel -> Object -> Parser a)
            -- ^ @id -> label -> properties -> Parser@
            -> Value
            -> Parser a
parseVertex upperParser (Object obj) = do
  vtype <- (obj .: "type") :: Parser Text
  guard (vtype == "vertex")
  vid <- obj .: "id"
  label <- obj .: "label"
  case HM.lookup "properties" obj of
   Just (Object prop_obj) -> upperParser vid label prop_obj
   _ -> empty
parseVertex _ _ = empty


