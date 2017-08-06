{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module: W5J.DB.TinkerPop.Parse
-- Description: parsers for responses from Gremlin Server
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module W5J.DB.TinkerPop.Parse
       ( ACompleteWhat(..),
         AVertexWhat(..),
         AVertexWhen(..),
         ioFromJSON
       ) where

import Control.Applicative ((<$>), (<*>), empty)
import Control.Monad (mapM, guard)
import Data.Aeson
  ( FromJSON(parseJSON), Object, (.:), Value(Object,Array), fromJSON,
    Result(Error,Success)
  )
import Data.Aeson.Types (Parser)
import Data.Foldable (toList)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (listToMaybe)
import Data.Text (Text)

import W5J.Aeson
  ( AWhat(AWhat), AWhen(AWhen), AWhere(AWhere),
    fromAWhat, fromAWhen, fromAWhere
  )
import W5J.Interval ((...))
import W5J.What (What(..))
import W5J.When (When(..))
import W5J.Where (Where(..))
import W5J.DB.TinkerPop.Error (parseError)

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

propOne :: FromJSON a => Object -> Text -> Parser a
propOne props name = unPropertyValue <$> unPropertyOne <$> (props .: name)

-- | Property value of LIST/SET cardinality
newtype PropertyMany a = PropertyMany { unPropertyMany :: [PropertyValue a] }
                       deriving (Show,Eq,Ord)

instance FromJSON a => FromJSON (PropertyMany a) where
  parseJSON (Array arr) = fmap (PropertyMany . map PropertyValue) $ mapM parseJSON $ toList arr
  parseJSON _ = empty

propMany :: FromJSON a => Object -> Text -> Parser [a]
propMany props name = map unPropertyValue <$> unPropertyMany <$> (props .: name)

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


-- | Aeson wrapper for complete 'What' data.
newtype ACompleteWhat = ACompleteWhat { unACompleteWhat :: What }


-- | Parse @[what_vertex, [when_from_vertices], [when_to_vertices], [where_vertices]]@
-- into a complete 'What' data.
instance FromJSON ACompleteWhat where
  parseJSON (Array arr') = do
    let arr = toList arr'
    guard (length arr == 3)
    let [what_v, when_from_vs, when_to_vs, where_vs] = arr
        getMWhen = fmap (fmap fromAWhen . listToMaybe . map unAVertexWhen) . parseJSON
    what <- fmap (fromAWhat . unAVertexWhat) $ parseJSON what_v
    mwhen_from <- getMWhen when_from_vs
    mwhen_to <- getMWhen when_to_vs
    wheres <- fmap (map (fromAWhere . unAVertexWhere)) $ parseJSON where_vs
    let mwhen_interval = (...) <$> mwhen_from <*> mwhen_to
    return $ ACompleteWhat $ what { whatWhen = mwhen_interval,
                                    whatWheres = wheres
                                  }
  parseJSON _ = empty
    


-- | Aeson wrapper of 'What' vertex.
newtype AVertexWhat = AVertexWhat { unAVertexWhat :: AWhat }

-- | Parse a TinkerPop vertex object into 'What'. Since the input is
-- only one vertex, 'whatWhen' and 'whatWheres' are empty.
instance FromJSON AVertexWhat where
  parseJSON = parseVertex f
    where
      f vid vlabel obj = do
        let p1 :: FromJSON a => Text -> Parser a
            p1 = propOne obj
            ps = propMany obj
        guard (vlabel == "what")
        fmap AVertexWhat
          $ AWhat vid
          <$> p1 "title"
          <*> pure Nothing
          <*> pure []
          <*> p1 "body"
          <*> ps "tags"
          <*> p1 "created_at"
          <*> p1 "updated_at"
        

-- | Aeson wrapper of 'When' vertex.
newtype AVertexWhen = AVertexWhen { unAVertexWhen :: AWhen }

instance FromJSON AVertexWhen where
  parseJSON = parseVertex f
    where
      f _ vlabel obj = do
        guard (vlabel == "when")
        fmap AVertexWhen $ AWhen
          <$> propOne obj "instant"
          <*> propOne obj "is_time_explicit"
          <*> propOne obj "time_zone"

-- | Aeson wrapper of 'Where' vertex.
newtype AVertexWhere = AVertexWhere { unAVertexWhere :: AWhere }

instance FromJSON AVertexWhere where
  parseJSON = parseVertex f
    where
      f vid vlabel obj = do
        guard (vlabel == "where")
        fmap (AVertexWhere . AWhere)
          $ Where (Just vid)
          <$> propOne obj "name"


ioFromJSON :: FromJSON a => Value -> IO a
ioFromJSON = toError . fromJSON
  where
    toError (Error err) = parseError err
    toError (Success a) = return a

