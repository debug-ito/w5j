{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module: W5J.DB.TinkerPop.GStep
-- Description: Gremlin steps/traversal types.
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module W5J.DB.TinkerPop.GStep
       ( -- * Type
         GTraversal,
         GremlinLike(..),
         ToGTraversal(..),
         GStep,
         (@.),
         Vertex,
         Edge,
         -- * GTraversal
         allVertices,
         vertexByID,
         -- * Conversion
         outVoid,
         -- * Filter step
         identity,
         filterL,
         filter,
         has,
         hasLabel,
         or,
         not,
         range,
         -- * Sorting step
         orderBy
       ) where

import Prelude hiding (or, filter, not)
import Control.Category (Category)
import qualified Control.Category as Category
import Data.Monoid ((<>), mconcat)
import qualified Data.Text as T
import Data.Void (Void)
import W5J.DB.TinkerPop.IO.Connection (Gremlin)

-- | GraphTraversal class object of TinkerPop.
newtype GTraversal s e = GTraversal { unGTraversal :: Gremlin }
                       deriving (Show)

-- TODO: make GTraversal a Category with flatMap step.

class GremlinLike g where
  unsafeFromGremlin :: Gremlin -> g
  toGremlin :: g -> Gremlin

instance GremlinLike (GTraversal s e) where
  unsafeFromGremlin = GTraversal
  toGremlin = unGTraversal

-- | Types that convert to 'GTraversal'.
class ToGTraversal g where
  toGTraversal :: g s e -> GTraversal s e

instance ToGTraversal GTraversal where
  toGTraversal = id

allVertices :: GTraversal Void Vertex
allVertices = unsafeFromGremlin "g.V()"

vertexByID :: Gremlin
              -- ^ Gremlin code for vertex ID.
           -> GTraversal Void Vertex
vertexByID vid = unsafeFromGremlin ("g.V(" <> vid <> ")")

infixl 5 @.

(@.) :: GTraversal a b -> GStep b c -> GTraversal a c
gt @. gs = unsafeFromGremlin (toGremlin gt <> toGremlin gs)


-- | A Gremlin Step (method call) that takes data @s@ from upstream
-- and emits data @e@ to downstream.
--
-- 'GStep' is a 'Category'. Use its methods to compose them.
--
-- 'GStep' is not an 'Eq', because it's difficult to define true
-- equality between Gremlin method calls. If we define it naively, it
-- might have conflict with 'Category' law.
newtype GStep s e = GStep { unGStep :: Gremlin }
                  deriving (Show)

-- | 'id' is 'identity'.
instance Category GStep where
  id = identity
  bc . ab = unsafeFromGremlin (unGStep ab <> unGStep bc)

-- | Call static method versions of the 'GStep' on @__@ class.
instance ToGTraversal GStep where
  toGTraversal step = unsafeFromGremlin ("__" <> toGremlin step)

instance GremlinLike (GStep s e) where
  unsafeFromGremlin = GStep
  toGremlin = unGStep

-- | Vertex in a TinkerPop graph.
data Vertex

-- | Edge in a TinkerPop graph.
data Edge

-- | Ignore the output.
outVoid :: GStep s e -> GStep s ()
outVoid = GStep . unGStep

-- | @.identity@ step.
identity :: GStep s s
identity = unsafeFromGremlin ".identity()"

-- | @.filter@ step with lambda block.
filterL :: Gremlin
          -- ^ Gremlin code inside filter's @{}@ block.
        -> GStep s s
filterL block = unsafeFromGremlin (".filter({" <> block <> "})")

-- | @.filter@ step with steps(traversal).
filter :: GStep s () -> GStep s s
filter step = unsafeFromGremlin (".filter(__" <> toGremlin step <> ")")

-- | @.has@ step.
has :: Gremlin
    -- ^ target
    -> Gremlin
    -- ^ expectation
    -> GStep s s
has target expec = unsafeFromGremlin (".has(" <> target <> ", " <> expec <> ")")

-- | @.hasLabel@ step
hasLabel :: Gremlin -- ^ expected label name
         -> GStep s s
hasLabel l = unsafeFromGremlin (".hasLabel(" <> l <> ")")

-- | @.or@ step.
or :: [GStep s e] -> GStep s s
or conds = unsafeFromGremlin (".or(" <> conds_g <> ")")
  where
    conds_g = T.intercalate ", " $ map toG conds
    toG cond = "__" <> toGremlin cond

-- | @.not@ step.
not :: GStep s e -> GStep s s
not cond = unsafeFromGremlin (".not(__" <> toGremlin cond <> ")")

-- | @.range@ step.
range :: Gremlin
      -- ^ min
      -> Gremlin
      -- ^ max
      -> GStep s s
range min_g max_g = unsafeFromGremlin (".range(" <> min_g <> ", " <> max_g <> ")")

-- | @.order@ and @.by@ steps
orderBy :: [(GStep s e, Gremlin)]
           -- ^ (accessor steps, comparator) of each @.by@
        -> GStep s s
orderBy bys = unsafeFromGremlin (".order()" <> bys_g)
  where
    bys_g = mconcat $ map toG bys
    toG (accessor, comparator) =
      ".by(__" <> toGremlin accessor <> ", " <> comparator <> ")"
