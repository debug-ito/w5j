{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module: W5J.DB.TinkerPop.GStep
-- Description: Gremlin steps/traversal types.
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module W5J.DB.TinkerPop.GStep
       ( -- * Type
         GStep,
         Vertex,
         Edge,
         -- * Conversion
         gremlinStep,
         unsafeGStep,
         outVoid,
         -- * Traversal source
         allVertices,
         vertexByID,
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
import W5J.DB.TinkerPop.IO.Connection (Gremlin)

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
  bc . ab = unsafeGStep (unGStep ab <> unGStep bc)

-- | Vertex in a TinkerPop graph.
data Vertex

-- | Edge in a TinkerPop graph.
data Edge

-- | Make arbitrary 'GStep'.
unsafeGStep :: Gremlin -> GStep s e
unsafeGStep = GStep

-- | Get Gremlin code of this step.
gremlinStep :: GStep s e -> Gremlin
gremlinStep = unGStep

-- | Ignore the output.
outVoid :: GStep s e -> GStep s ()
outVoid = GStep . unGStep

-- todo: make the source type Void, so that it won't compose with
-- anything else.
allVertices :: GStep () Vertex
allVertices = unsafeGStep "g.V()"

vertexByID :: Gremlin
              -- ^ Gremlin code for vertex ID.
           -> GStep () Vertex
vertexByID vid = unsafeGStep ("g.V(" <> vid <> ")")

-- | @.identity@ step.
identity :: GStep s s
identity = unsafeGStep ".identity()"

-- | @.filter@ step with lambda block.
filterL :: Gremlin
          -- ^ Gremlin code inside filter's @{}@ block.
        -> GStep s s
filterL block = unsafeGStep (".filter({" <> block <> "})")

-- | @.filter@ step with steps(traversal).
filter :: GStep s () -> GStep s s
filter step = unsafeGStep (".filter(__" <> gremlinStep step <> ")")

-- | @.has@ step.
has :: Gremlin
    -- ^ target
    -> Gremlin
    -- ^ expectation
    -> GStep s s
has target expec = unsafeGStep (".has(" <> target <> ", " <> expec <> ")")

-- | @.hasLabel@ step
hasLabel :: Gremlin -- ^ expected label name
         -> GStep s s
hasLabel l = unsafeGStep (".hasLabel(" <> l <> ")")

-- | @.or@ step.
or :: [GStep s ()] -> GStep s s
or conds = unsafeGStep (".or(" <> conds_g <> ")")
  where
    conds_g = T.intercalate ", " $ map toG conds
    toG cond = "__" <> gremlinStep cond

-- | @.not@ step.
not :: GStep s () -> GStep s s
not cond = unsafeGStep (".not(__" <> gremlinStep cond <> ")")

-- | @.range@ step.
range :: Gremlin
      -- ^ min
      -> Gremlin
      -- ^ max
      -> GStep s s
range min_g max_g = unsafeGStep (".range(" <> min_g <> ", " <> max_g <> ")")

-- | @.order@ and @.by@ steps
orderBy :: [(GStep s (), Gremlin)]
           -- ^ (accessor steps, comparator) of each @.by@
        -> GStep s s
orderBy bys = unsafeGStep (".order()" <> bys_g)
  where
    bys_g = mconcat $ map toG bys
    toG (accessor, comparator) =
      ".by(__" <> gremlinStep accessor <> ", " <> comparator <> ")"
