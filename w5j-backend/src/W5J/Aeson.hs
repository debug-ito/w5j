{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module: W5J.Aeson
-- Description: standard JSON representation of data types
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module W5J.Aeson
       ( AInterval(..),
         ATimeInstant(..)
       ) where

import Control.Applicative ((<$>), (<*>), empty)
import Data.Aeson (ToJSON(..), FromJSON(..), object, (.:), Value(..))

import W5J.Interval (Interval, inf, sup, (...))
import W5J.Time (TimeInstant, toEpochMsec, fromEpochMsec)

newtype AInterval a = AInterval (Interval a)

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

instance ToJSON ATimeInstant where
  toJSON (ATimeInstant t) = toJSON $ toEpochMsec t

instance FromJSON ATimeInstant where
  parseJSON = fmap (ATimeInstant . fromEpochMsec) . parseJSON
