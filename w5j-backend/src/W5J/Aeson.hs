{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
-- |
-- Module: W5J.Aeson
-- Description: standard JSON representation of data types
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- These JSON representations are used by the frontend <-> backend
-- interface (both direction) and backend -> gremlin interface
-- (one-way).
--
-- For backend <- gremlin interface, see "W5J.DB.TinkerPop.Parse".
module W5J.Aeson
       ( -- * What
         AWhat(..),
         toAWhat,
         fromAWhat,
         -- * When
         AWhen(..),
         toAWhen,
         fromAWhen,
         -- * Where
         AWhere(..),
         toAWhere,
         fromAWhere,
         -- * basics
         AInterval(..),
         ATimeInstant(..),
         ATimeZone(..)
       ) where

import Control.Applicative ((<$>), (<*>), empty)
import Data.Aeson
  ( ToJSON(..), FromJSON(..), object, Value(..),
    genericToEncoding, genericToJSON, genericParseJSON
  )
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonType
import Data.Greskell.GraphSON
  ( FromGraphSON(..), (.:), GValue, parseUnwrapAll,
    parseJSONViaGValue
  )
import Data.Text (Text, unpack)
import GHC.Generics (Generic)

import W5J.Interval (Interval, inf, sup, (...), mapInterval)
import W5J.Time
  ( TimeInstant, toEpochMsec, fromEpochMsec,
    TimeZone, tzToString, tzFromString
  )
import W5J.What (What(..), WhatID, Tag)
import W5J.When (When(..))
import W5J.Where (Where(..), WhereID)

newtype AInterval a = AInterval { unAInterval :: Interval a }
                    deriving (Eq,Show,Ord)

instance ToJSON a => ToJSON (AInterval a) where
  toJSON (AInterval int) = object [ ("from", toJSON $ inf int),
                                    ("to", toJSON $ sup int)
                                  ]

instance (FromJSON a, Ord a) => FromGraphSON (AInterval a) where
  parseGraphSON v = fromMap =<< parseGraphSON v
    where
      fromMap o = fmap AInterval
                  $ (...)
                  <$> (parseUnwrapAll =<< o .: "from")
                  <*> (parseUnwrapAll =<< o .: "to")

instance (FromJSON a, Ord a) => FromJSON (AInterval a) where
  parseJSON = parseJSONViaGValue

newtype ATimeInstant = ATimeInstant { unATimeInstant ::TimeInstant }
                     deriving (Eq,Ord,Show)

instance ToJSON ATimeInstant where
  toJSON (ATimeInstant t) = toJSON $ toEpochMsec t

instance FromGraphSON ATimeInstant where
  parseGraphSON = fmap (ATimeInstant . fromEpochMsec) . parseGraphSON

instance FromJSON ATimeInstant where
  parseJSON = parseJSONViaGValue


newtype ATimeZone = ATimeZone { unATimeZone :: TimeZone }
                  deriving (Eq,Ord,Show)

instance ToJSON ATimeZone where
  toJSON (ATimeZone tz) = toJSON $ tzToString tz

instance FromGraphSON ATimeZone where
  parseGraphSON v = (maybe empty (return . ATimeZone) . tzFromString . unpack) =<< parseGraphSON v

instance FromJSON ATimeZone where
  parseJSON = parseJSONViaGValue


aesonOpt :: AesonType.Options
aesonOpt = AesonType.defaultOptions
           { AesonType.fieldLabelModifier = tail
           }

data AWhen =
  AWhen
  { _instant :: !ATimeInstant,
    _is_time_explicit :: !Bool,
    _time_zone :: !ATimeZone
  }
  deriving (Eq,Show,Generic)

instance ToJSON AWhen where
  toJSON = genericToJSON aesonOpt
  toEncoding = genericToEncoding aesonOpt

instance FromJSON AWhen where
  parseJSON = genericParseJSON aesonOpt

instance Ord AWhen where
  compare a b = compare (fromAWhen a) (fromAWhen b)

toAWhen :: When -> AWhen
toAWhen (When a b c) = AWhen (ATimeInstant a) b (ATimeZone c)

fromAWhen :: AWhen -> When
fromAWhen (AWhen (ATimeInstant a) b (ATimeZone c)) = When a b c


newtype AWhere = AWhere Where
                 deriving (Show,Eq,Ord)

instance ToJSON AWhere where
  toJSON (AWhere w) = object [ ("where_id", toJSON $ whereId w),
                               ("name", toJSON $ whereName w)
                             ]

instance FromJSON AWhere where
  parseJSON (Object o) = fmap AWhere
                         $ Where <$> (o Aeson..: "where_id") <*> (o Aeson..: "name")
  parseJSON _ = empty

toAWhere :: Where -> AWhere
toAWhere = AWhere

fromAWhere :: AWhere -> Where
fromAWhere (AWhere w) = w


data AWhat =
  AWhat
  { _what_id :: !WhatID,
    _title :: !Text,
    _when :: !(Maybe (AInterval AWhen)),
    _wheres :: ![AWhere],
    _body :: !Text,
    _tags :: ![Tag],
    _created_at :: !ATimeInstant,
    _updated_at :: !ATimeInstant
  }
  deriving (Show,Eq,Generic)

instance ToJSON AWhat where
  toJSON = genericToJSON aesonOpt
  toEncoding = genericToEncoding aesonOpt

instance FromJSON AWhat where
  parseJSON = genericParseJSON aesonOpt

toAWhat :: What -> AWhat
toAWhat w = AWhat (whatId w) (whatTitle w) v_when v_wheres (whatBody w)
            (whatTags w) (ATimeInstant $ whatCreatedAt w) (ATimeInstant $ whatUpdatedAt w)
  where
    v_when = fmap AInterval $ fmap (mapInterval toAWhen) $ whatWhen w
    v_wheres = map toAWhere $ whatWheres w

fromAWhat :: AWhat -> What
fromAWhat aw = What (_what_id aw) (_title aw) v_when v_wheres (_body aw)
               (_tags aw) (unATimeInstant $ _created_at aw) (unATimeInstant $ _updated_at aw)
  where
    v_when = fmap (mapInterval fromAWhen) $ fmap unAInterval $ _when aw
    v_wheres = map fromAWhere $ _wheres aw
    
