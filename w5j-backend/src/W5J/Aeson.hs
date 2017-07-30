{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
-- |
-- Module: W5J.Aeson
-- Description: standard JSON representation of data types
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module W5J.Aeson
       ( -- * When
         AWhen,
         toAWhen,
         fromAWhen,
         -- * Where
         AWhere,
         toAWhere,
         fromAWhere,
         -- * basics
         AInterval(..),
         ATimeInstant(..),
         ATimeZone(..)
       ) where

import Control.Applicative ((<$>), (<*>), empty)
import Data.Aeson
  ( ToJSON(..), FromJSON(..), object, (.:), Value(..),
    genericToEncoding, genericParseJSON
  )
import qualified Data.Aeson.Types as AesonType
import GHC.Generics (Generic)

import W5J.Interval (Interval, inf, sup, (...))
import W5J.Time
  ( TimeInstant, toEpochMsec, fromEpochMsec,
    TimeZone, tzToString, tzFromString
  )
import W5J.What (What(..), WhatID, Tag)
import W5J.When (When(..))
import W5J.Where (Where(..), WhereID)

newtype AInterval a = AInterval (Interval a)
                    deriving (Eq,Show,Ord)

instance ToJSON a => ToJSON (AInterval a) where
  toJSON (AInterval int) = object [ ("from", toJSON $ inf int),
                                    ("to", toJSON $ sup int)
                                  ]

instance (FromJSON a, Ord a) => FromJSON (AInterval a) where
  parseJSON (Object o) = fmap AInterval
                         $ (...)
                         <$> (o .: "from")
                         <*> (o .: "to")
  parseJSON _ = empty

newtype ATimeInstant = ATimeInstant TimeInstant
                     deriving (Eq,Ord,Show)

instance ToJSON ATimeInstant where
  toJSON (ATimeInstant t) = toJSON $ toEpochMsec t

instance FromJSON ATimeInstant where
  parseJSON = fmap (ATimeInstant . fromEpochMsec) . parseJSON


newtype ATimeZone = ATimeZone TimeZone
                  deriving (Eq,Ord,Show)

instance ToJSON ATimeZone where
  toJSON (ATimeZone tz) = toJSON $ tzToString tz

instance FromJSON ATimeZone where
  parseJSON v = (maybe empty (return . ATimeZone) . tzFromString) =<< parseJSON v

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
  toEncoding = genericToEncoding aesonOpt

instance FromJSON AWhen where
  parseJSON = genericParseJSON aesonOpt

toAWhen :: When -> AWhen
toAWhen (When a b c) = AWhen (ATimeInstant a) b (ATimeZone c)

fromAWhen :: AWhen -> When
fromAWhen (AWhen (ATimeInstant a) b (ATimeZone c)) = When a b c


newtype AWhere = AWhere Where

instance ToJSON AWhere where
  toJSON (AWhere w) = object [ ("where_id", toJSON $ whereId w),
                               ("name", toJSON $ whereName w)
                             ]

instance FromJSON AWhere where
  parseJSON (Object o) = fmap AWhere
                         $ Where <$> (o .: "where_id") <*> (o .: "name")
  parseJSON _ = empty

toAWhere :: Where -> AWhere
toAWhere = AWhere

fromAWhere :: AWhere -> Where
fromAWhere (AWhere w) = w


