{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
-- |
-- Module: W5J.DB.TinkerPop.Query.What
-- Description: queries for What
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module W5J.DB.TinkerPop.Query.What
       ( QueryWhat,
         buildQuery,
         QOrderBy(..),
         QCond(..),
         WhatVertex
       ) where

import Control.Category ((>>>))
import Control.Monad (void)
import Data.Aeson (toJSON)
import Data.Greskell
  ( Binder, GTraversal, Transform, Greskell, P,
    ($.), liftWalk,
    source, vertices,
    newBind,
    unsafeFunCall, toGremlin, unsafeGreskell,
    gOr, gHas2, gHas2', pEq, gFilter, gOut', gHasId, gOrderBy, gHasLabel,
    ByComparator(..), pjTraversal,
    Element(..), Vertex, AesonVertexProperty
  )
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Void (Void)

-- import W5J.DB.TinkerPop.GBuilder (GBuilder, newBind, GScript)
-- import W5J.DB.TinkerPop.GScript (gRaw, gFunCall, gLiteral)
-- import W5J.DB.TinkerPop.GStep
--   ( (@.), toGScript, toGTraversal, liftType,
--     allVertices', gHasLabel', gHas, gHasId', gOrderBy,
--     unsafeGTraversal, gValues, gFilter, gOut, gOr,
--     GTraversal, Transform, Vertex, Element
--   )
import W5J.DB.TinkerPop.Query.Common
  ( Query, QOrder(..),
    buildQueryWith, orderComparator
  )
import W5J.What (WhatID)
import W5J.When (When)
import W5J.Where (WhereID)

type QueryWhat = Query QCond QOrderBy

-- | order specifier for What data.
data QOrderBy = QOrderByWhen
              deriving (Show,Eq,Ord)

-- | comparator symbols for conditions. (this may be in
-- "W5J.DB.TinkerPop.Query.Common" module.)
data QComparator = QCompLte
                 | QCompGte
                 deriving (Show,Eq,Ord)

-- | condition symbols about 'whatWhen'.
data QCondWhenTerm = QCondWhenFrom
                   | QCondWhenTo
                   deriving (Show,Eq,Ord)

-- | condition specifier for What data
data QCond = QCondTerm Text
             -- ^ free term search entry for title, body, tags.
           | QCondTag Text
             -- ^ exact match for tags.
           | QCondWhereID WhereID
             -- ^ match for 'whatWheres' by ID.
           | QCondWhereName Text
             -- ^ match for 'whatWheres' by name.
           | QCondWhenExists
             -- ^ match if 'whatWhen' is not 'Nothing'.
           | QCondWhen QCondWhenTerm QComparator When
             -- ^ compare 'whatWhen' with the given constant
             -- 'When'. This implies the 'whatWhen' is not 'Nothing'.
           deriving (Show,Eq,Ord)

-- | A 'Vertex' for \"what\" data.
data WhatVertex

-- TODO: elementIdとかelementLabelとかを実装できるよう、WhatVertexを具体型にしたほうがいい。
-- つか、ParseモジュールのAVertexWhatを使えばいい。
instance Element WhatVertex where
  type ElementID WhatVertex = WhatID
  type ElementProperty WhatVertex = AesonVertexProperty

instance Vertex WhatVertex

buildQuery :: QueryWhat -> Binder (GTraversal Transform Void WhatVertex)
buildQuery query = do
  traversal <- buildQueryWith buildCond buildOrder query
  return $ liftWalk traversal $. gHasLabel (pEq "what") $. vertices [] $ source "g"
  where
    -- For textContains predicate, see http://s3.thinkaurelius.com/docs/titan/1.0.0/index-parameters.html
    pTextContains :: Greskell Text -> Greskell (P Text)
    pTextContains t = unsafeFunCall "textContains" [toGremlin t]
    buildCond (QCondTerm t) = do
      vt <- newBind t
      return $ gOr
        [ gHas2' "title" (pTextContains vt),
          gHas2' "body"  (pTextContains vt),
          gHas2' "tags"  (pEq vt)
        ]
    buildCond (QCondTag t) = do
      vt <- newBind t
      return $ gHas2 "tags" (pEq vt)
    buildCond (QCondWhereID where_id) = do -- TODO: こいつのテストから。いろいろあったけどようやく再開かな？ ていうか、まずgreskellをある程度モノにしよう。
      vid <- newBind where_id
      return $ gFilter (gOut' ["where"] >>> gHasId (pEq (fmap toJSON $ vid)))
    buildCond (QCondWhereName _) = undefined -- TODO
    buildCond (QCondWhenExists) = undefined -- TODO
    buildCond (QCondWhen _ _ _) = undefined -- TODO
    buildOrder order QOrderByWhen = undefined
    -- -- TODO: optionalTじゃなくてfold使うんじゃないか？
    --   return $ gOrderBy [byWhen "when_from", byWhen "when_to", commonBy]
    --   where
    --     byWhen edge_label =
    --       (unsafeGTraversal (gFunCall "optionalT" [gFunCall "out" [gLiteral edge_label]]), comparator)
    --     comparator = case order of
    --       QOrderAsc -> unsafeGreskell "compareOptWhenVertices"
    --       QOrderDesc -> unsafeGreskell "compareOptWhenVertices.reversed()"
    --     commonBy =
    --       (toGTraversal $ void $ gValues ["updated_at"], orderComparator order)
      
