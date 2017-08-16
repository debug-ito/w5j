{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module: W5J.DB.TinkerPop.GStep
-- Description: Gremlin steps/traversal types.
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module W5J.DB.TinkerPop.GStep
       ( -- * Types
         -- ** Gremlin Traversals and Steps
         GStep,
         GTraversal,
         GremlinLike(..),
         ToGTraversal(..),
         -- ** Step markers,
         Filter,
         Any,
         -- ** Types in Gremlin
         Vertex,
         Edge,
         Element,
         PropertyValue,
         ElementID,
         -- * GTraversal
         (@.),
         allVertices,
         vertexByID,
         unsafeGTraversal,
         -- * GStep
         unsafeGStep,
         -- ** Filter step
         identity,
         filterL,
         filter,
         has,
         hasLabel,
         hasId,
         or,
         not,
         range,
         -- ** Sorting step
         orderBy,
         -- ** Transformation step
         flatMap,
         values,
         -- ** Graph traversal step
         out,
         outE,
         inS,
         inE
       ) where

import Prelude hiding (or, filter, not)
import Control.Category (Category)
-- (below) to import Category methods without conflict with Prelude
import qualified Control.Category as Category
import Data.Monoid ((<>), mconcat)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import W5J.DB.TinkerPop.IO.Connection (Gremlin)


-- | A Gremlin Step (method call) that takes data @s@ from upstream
-- and emits data @e@ to downstream. Type @c@ is a marker to describe
-- the step.
--
-- 'GStep' is a 'Category'. Use its methods to compose them.
--
-- 'GStep' is not an 'Eq', because it's difficult to define true
-- equality between Gremlin method calls. If we define it naively, it
-- might have conflict with 'Category' law.
newtype GStep c s e = GStep { unGStep :: Gremlin }
                    deriving (Show)

-- | 'id' is 'identity'.
instance Category (GStep c) where
  id = identity
  bc . ab = unsafeFromGremlin (unGStep ab <> unGStep bc)

-- | Unsafely convert output type
instance Functor (GStep c s) where
  fmap _ = GStep . unGStep

-- | Call static method versions of the 'GStep' on @__@ class.
instance ToGTraversal GStep where
  toGTraversal step = unsafeFromGremlin ("__" <> toGremlin step)
  forgetFilter = GStep . unGStep

instance GremlinLike (GStep c s e) where
  unsafeFromGremlin = GStep
  toGremlin = unGStep


-- | GraphTraversal class object of TinkerPop.
--
-- 'GTraversal' is practically the same as 'GStep'. 'GTraversal' is a
-- Java-object in Gremlin domain, while 'GStep' is a chain of method
-- calls.
newtype GTraversal c s e = GTraversal { unGTraversal :: Gremlin }
                         deriving (Show)
                                  
-- | 'id' is @__.identity()@. '(.)' compose 'GTraversal's by
-- @.flatMap@ step.
instance Category (GTraversal c) where
  id = toGTraversal identity
  a . b = b @. flatMap a

-- | Unsafely convert output type.
instance Functor (GTraversal c s) where
  fmap _ = GTraversal . unGTraversal

-- | Something that is isomorphic to 'Gremlin'.
class GremlinLike g where
  unsafeFromGremlin :: Gremlin -> g
  toGremlin :: g -> Gremlin

instance GremlinLike (GTraversal c s e) where
  unsafeFromGremlin = GTraversal
  toGremlin = unGTraversal

-- | Types that can convert to 'GTraversal'.
class ToGTraversal g where
  toGTraversal :: g c s e -> GTraversal c s e
  forgetFilter :: g Filter s e -> g Any s e
  -- ^ Treat a filtering step/traversal as a general step. Use this
  -- for type matching.

instance ToGTraversal GTraversal where
  toGTraversal = id
  forgetFilter = GTraversal . unGTraversal


-- | Type marker for filtering steps.
--
-- A filtering step is a step that does filtering only. It takes input
-- and outputs some of them without any modification or traversal
-- actions. Modification of orders (like sorting) is included as
-- filtering.
data Filter

-- | Type marker for any steps, whether it's filtering or not.
data Any

unsafeGTraversal :: Gremlin -> GTraversal c s e
unsafeGTraversal = GTraversal

allVertices :: GTraversal Any Void Vertex
allVertices = unsafeFromGremlin "g.V()"

vertexByID :: Gremlin
              -- ^ Gremlin code for vertex ID.
           -> GTraversal Any Void Vertex
vertexByID vid = unsafeFromGremlin ("g.V(" <> vid <> ")")

infixl 5 @.

(@.) :: GTraversal c a b -> GStep c b d -> GTraversal c a d
gt @. gs = unsafeFromGremlin (toGremlin gt <> toGremlin gs)


-- | Vertex in a TinkerPop graph.
data Vertex

-- | Edge in a TinkerPop graph.
data Edge

-- | Element interface in a TinkerPop graph.
class Element e where
  getPropertyValue :: Gremlin -> e -> PropertyValue
  getPropertyValue = error "This is a phantom method to suppress redundant-constaint warning. Do not evaluate this!"
  getElementID :: e -> ElementID
  getElementID = error "This is a phantom method to suppress redundant-constaint warning. Do not evaluate this!"
  getLabel :: e -> Text
  getLabel = error "This is a phantom method to suppress redundant-constaint warning. Do not evaluate this!"

instance Element Vertex
instance Element Edge

-- | Value object in a TinkerPop graph.
data PropertyValue

-- | ID object type for Elements
data ElementID

unsafeGStep :: Gremlin -> GStep c s e
unsafeGStep = GStep

-- | @.identity@ step.
identity :: GStep Filter s s
identity = unsafeFromGremlin ".identity()"

-- | @.filter@ step with lambda block.
filterL :: Gremlin
          -- ^ Gremlin code inside filter's @{}@ block.
        -> GStep Filter s s
filterL block = unsafeFromGremlin (".filter({" <> block <> "})")

-- | @.filter@ step with steps(traversal).
filter :: ToGTraversal g => g c s e -> GStep Filter s s
filter step = unsafeFromGremlin (".filter(" <> (toGremlin $ toGTraversal step) <> ")")

unsafeFilterStep :: (s -> a) -> Gremlin -> GStep Filter s s
unsafeFilterStep _ = unsafeGStep

-- | @.has@ step.
has :: Element s
    => Gremlin -- ^ target
    -> Gremlin -- ^ expectation
    -> GStep Filter s s
has target expec = unsafeFilterStep (getPropertyValue target) (".has(" <> target <> ", " <> expec <> ")")

genericMultiArgFilter :: Gremlin -- ^ method name
                      -> (s -> a) -- ^ phantom filtering accessor
                      -> [Gremlin] -- ^ arguments
                      -> GStep Filter s s
genericMultiArgFilter method_name f args = unsafeFilterStep f ("." <> method_name <> "(" <> args_g <> ")")
  where
    args_g = T.intercalate ", " args

-- | @.hasLabel@ step
hasLabel :: Element s
         => [Gremlin] -- ^ expected label names
         -> GStep Filter s s
hasLabel = genericMultiArgFilter "hasLabel" getLabel

hasId :: Element s
      => [Gremlin] -- ^ expected IDs
      -> GStep Filter s s
hasId = genericMultiArgFilter "hasId" getElementID

-- | @.or@ step.
or :: ToGTraversal g => [g c s e] -> GStep Filter s s
or conds = unsafeFromGremlin (".or(" <> conds_g <> ")")
  where
    conds_g = T.intercalate ", " $ map toG conds
    toG cond = toGremlin $ toGTraversal cond

-- | @.not@ step.
not :: ToGTraversal g => g c s e -> GStep Filter s s
not cond = unsafeFromGremlin (".not(" <> (toGremlin $ toGTraversal cond) <> ")")

-- | @.range@ step.
range :: Gremlin
      -- ^ min
      -> Gremlin
      -- ^ max
      -> GStep Filter s s
range min_g max_g = unsafeFromGremlin (".range(" <> min_g <> ", " <> max_g <> ")")

-- | @.order@ and @.by@ steps
orderBy :: ToGTraversal g
        => [(g c s e, Gremlin)]
           -- ^ (accessor steps, comparator) of each @.by@
        -> GStep Filter s s
orderBy bys = unsafeFromGremlin (".order()" <> bys_g)
  where
    bys_g = mconcat $ map toG bys
    toG (accessor, comparator) =
      ".by(" <> (toGremlin $ toGTraversal accessor) <> ", " <> comparator <> ")"

-- | @.flatMap@ step
flatMap :: ToGTraversal g => g c s e -> GStep c s e
flatMap gt = unsafeFromGremlin (".flatMap(" <> (toGremlin $ toGTraversal gt) <> ")")

unsafeTransformStep :: (a -> b) -> Gremlin -> GStep Any a b
unsafeTransformStep _ = unsafeGStep

-- | @.values@ step.
values :: Element s
       => [Gremlin]
       -- ^ property keys
       -> GStep Any s PropertyValue
values keys = unsafeTransformStep (getPropertyValue "DUMMY") (".values(" <> keys_g <> ")")
  where
    keys_g = T.intercalate ", " keys

genericTraversalStep :: Gremlin -> [Gremlin] -> GStep Any Vertex e
genericTraversalStep method_name edge_labels =
  unsafeFromGremlin ("." <> method_name <> "(" <> labels_g <> ")")
  where
    labels_g = T.intercalate ", " edge_labels

-- | @.out@ step
out :: [Gremlin] -- ^ edge labels
    -> GStep Any Vertex Vertex
out = genericTraversalStep "out"

-- | @.outE@ step
outE :: [Gremlin] -- ^ edge labels
     -> GStep Any Vertex Edge
outE = genericTraversalStep "outE"

-- | @.in@ step (@in@ is reserved by Haskell..)
inS :: [Gremlin] -- ^ edge labels
    -> GStep Any Vertex Vertex
inS = genericTraversalStep "in"

inE :: [Gremlin] -- ^ edge labels
    -> GStep Any Vertex Edge
inE = genericTraversalStep "inE"
