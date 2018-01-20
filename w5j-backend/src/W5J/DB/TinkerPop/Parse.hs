{-# LANGUAGE OverloadedStrings #-}
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
  ( AesonVertex(..), AesonVertexProperty(..), lookupOneValue, lookupListValues,
    GraphSON(gsonValue), PropertyMapList
  )
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (listToMaybe)
import Data.Monoid (mempty)
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


-- | Aeson wrapper for complete 'What' data.
newtype ACompleteWhat = ACompleteWhat { unACompleteWhat :: What }


-- | Parse @[what_vertex, [when_from_vertices], [when_to_vertices], [where_vertices]]@
-- into a complete 'What' data.
instance FromJSON ACompleteWhat where
  parseJSON (Array arr') = do
    let arr = toList arr'
    guard (length arr == 4)
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

parseVPOne :: FromJSON v => AesonVertex -> Text -> Parser v
parseVPOne av key = (parseJSON . gsonValue) =<< (maybe empty pure $ lookupOneValue key $ avProperties av)

parseVPList :: FromJSON v => AesonVertex -> Text -> Parser [v]
parseVPList av key = mapM parseJSON $ map gsonValue $ lookupListValues key $ avProperties av

-- | Aeson wrapper of 'What' vertex.
newtype AVertexWhat = AVertexWhat { unAVertexWhat :: AWhat }

-- | Parse a TinkerPop vertex object into 'What'. Since the input is
-- only one vertex, 'whatWhen' and 'whatWheres' are empty.
instance FromJSON AVertexWhat where
  parseJSON v = fromAesonVertex =<< parseJSON v
    where
      fromAesonVertex av = do
        guard (avLabel av == "what")
        let p1 :: FromJSON v => Text -> Parser v
            p1 = parseVPOne av
            ps = parseVPList av
        fmap AVertexWhat
          $ AWhat
          <$> (parseJSON $ gsonValue $ avId av)
          <*> p1 "title"
          <*> pure Nothing
          <*> pure []
          <*> p1 "body"
          <*> ps "tags"
          <*> p1 "created_at"
          <*> p1 "updated_at"
        

-- | Aeson wrapper of 'When' vertex.
newtype AVertexWhen = AVertexWhen { unAVertexWhen :: AWhen }

instance FromJSON AVertexWhen where
  parseJSON v = fromAesonVertex =<< parseJSON v
    where
      fromAesonVertex av = do
        guard (avLabel av == "when")
        fmap AVertexWhen $ AWhen
          <$> parseVPOne av "instant"
          <*> parseVPOne av "is_time_explicit"
          <*> parseVPOne av "time_zone"

-- | Aeson wrapper of 'Where' vertex.
newtype AVertexWhere = AVertexWhere { unAVertexWhere :: AWhere }

instance FromJSON AVertexWhere where
  parseJSON v = fromAesonVertex =<< parseJSON v
    where
      fromAesonVertex av = do
        guard (avLabel av == "where")
        fmap (AVertexWhere . AWhere)
          $ Where
          <$> (fmap Just $ parseJSON $ gsonValue $ avId av)
          <*> parseVPOne av "name"


ioFromJSON :: FromJSON a => Value -> IO a
ioFromJSON = toError . fromJSON
  where
    toError (Error err) = parseError err
    toError (Success a) = return a

