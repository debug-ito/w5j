{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
-- |
-- Module: W5J.DB.TinkerPop.Parse
-- Description: parsers for responses from Gremlin Server
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module W5J.DB.TinkerPop.Parse
       ( -- * Main parsers
         ACompleteWhat(..),
         -- * Parsing utils
         ioFromJSON,
         -- * Basic parsers
         AVertexWhat(..),
         AVertexWhen(..),
         AVertexWhere(..)
       ) where

import Control.Applicative ((<$>), (<*>), (<|>), empty)
import Control.Monad (mapM, guard)
import Data.Aeson
  ( FromJSON(parseJSON), Object, (.:), Value(Object,Array), fromJSON,
    Result(Error,Success)
  )
import Data.Aeson.Types (Parser)
import Data.Foldable (toList)
import Data.Greskell
  ( AVertex(..), AVertexProperty(..), parseOneValue, parseListValues,
    GraphSON(gsonValue), PropertyMapList,
    Element(..), Vertex,
    FromGraphSON(..), parseJSONViaGValue
  )
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Monoid (mempty)
import Data.Text (Text)
import Data.Vector ((!), (!?))
import qualified Data.Vector as V

import W5J.Aeson
  ( AWhat(AWhat,_what_id), AWhen(AWhen), AWhere(AWhere),
    fromAWhat, fromAWhen, fromAWhere
  )
import W5J.Interval ((...))
import W5J.What (What(..), WhatID)
import W5J.When (When(..))
import W5J.Where (Where(..), WhereID)
import W5J.DB.TinkerPop.Error (parseError)


-- | Aeson wrapper for complete 'What' data.
newtype ACompleteWhat = ACompleteWhat { unACompleteWhat :: What }



-- | Parse @[what_vertex, [when_from_vertices], [when_to_vertices], [where_vertices]]@
-- into a complete 'What' data.
instance FromGraphSON ACompleteWhat where
  parseGraphSON v = fromV =<< parseGraphSON v
    where
      getMWhen = fmap (fromAWhen . unAVertexWhen) . (!? 0)
      fromV vec = do
        guard (V.length vec == 4)
        what <- fmap (fromAWhat . unAVertexWhat) $ parseGraphSON (vec ! 0)
        mwhen_from <- fmap getMWhen $ parseGraphSON (vec ! 1)
        mwhen_to <- fmap getMWhen $ parseGraphSON (vec ! 2)
        wheres <- fmap (map (fromAWhere . unAVertexWhere)) $ parseGraphSON (vec ! 3)
        return $ ACompleteWhat $ what { whatWhen = (...) <$> mwhen_from <*> mwhen_to,
                                        whatWheres = wheres
                                      }

instance FromJSON ACompleteWhat where
  parseJSON = parseJSONViaGValue

parseVPOne :: FromGraphSON v => AVertex -> Text -> Parser v
parseVPOne av key = parseOneValue key $ avProperties av

parseVPList :: FromGraphSON v => AVertex -> Text -> Parser [v]
parseVPList av key = parseListValues key $ avProperties av

-- | Aeson wrapper of 'What' vertex.
newtype AVertexWhat = AVertexWhat { unAVertexWhat :: AWhat }

instance FromJSON AVertexWhat where
  parseJSON = parseJSONViaGValue

-- | Parse a TinkerPop vertex object into 'What'. Since the input is
-- only one vertex, 'whatWhen' and 'whatWheres' are empty.
instance FromGraphSON AVertexWhat where
  parseGraphSON v = fromAVertex =<< parseGraphSON v
    where
      fromAVertex av = do
        guard (avLabel av == "what")
        let p1 :: FromGraphSON v => Text -> Parser v
            p1 = parseVPOne av
            ps = parseVPList av
        fmap AVertexWhat
          $ AWhat
          <$> (parseGraphSON $ avId av)
          <*> p1 "title"
          <*> pure Nothing
          <*> pure []
          <*> p1 "body"
          <*> ps "tags"
          <*> p1 "created_at"
          <*> p1 "updated_at"

instance Element AVertexWhat where
  type ElementID AVertexWhat = WhatID
  type ElementProperty AVertexWhat = AVertexProperty

instance Vertex AVertexWhat

-- | Aeson wrapper of 'When' vertex.
newtype AVertexWhen = AVertexWhen { unAVertexWhen :: AWhen }

instance Element AVertexWhen where
  type ElementID AVertexWhen = Value
  type ElementProperty AVertexWhen = AVertexProperty

instance Vertex AVertexWhen

instance FromJSON AVertexWhen where
  parseJSON = parseJSONViaGValue

instance FromGraphSON AVertexWhen where
  parseGraphSON v = fromAVertex =<< parseGraphSON v
    where
      fromAVertex av = do
        guard (avLabel av == "when")
        fmap AVertexWhen $ AWhen
          <$> parseVPOne av "instant"
          <*> parseVPOne av "is_time_explicit"
          <*> parseVPOne av "time_zone"

-- | Aeson wrapper of 'Where' vertex.
newtype AVertexWhere = AVertexWhere { unAVertexWhere :: AWhere }

instance Element AVertexWhere where
  type ElementID AVertexWhere = WhereID
  type ElementProperty AVertexWhere = AVertexProperty

instance Vertex AVertexWhere

instance FromJSON AVertexWhere where
  parseJSON = parseJSONViaGValue

instance FromGraphSON AVertexWhere where
  parseGraphSON v = fromAVertex =<< parseGraphSON v
    where
      fromAVertex av = do
        guard (avLabel av == "where")
        fmap (AVertexWhere . AWhere)
          $ Where
          <$> (fmap Just $ parseGraphSON $ avId av)
          <*> parseVPOne av "name"


ioFromJSON :: FromJSON a => Value -> IO a
ioFromJSON = toError . fromJSON
  where
    toError (Error err) = parseError err
    toError (Success a) = return a

