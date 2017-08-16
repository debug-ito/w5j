{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module: W5J.DB.TinkerPop.GStep
-- Description: Gremlin steps/traversal types.
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module W5J.DB.TinkerPop.GStep
       ( -- * Type
         -- ** Gremlin Traversals and Steps
         GTraversal,
         GremlinLike(..),
         ToGTraversal(..),
         GStep,
         -- ** Types in Gremlin
         Vertex,
         Edge,
         Element,
         PropertyValue,
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
import qualified Control.Category as Category
import Data.Monoid ((<>), mconcat)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import W5J.DB.TinkerPop.IO.Connection (Gremlin)

-- | GraphTraversal class object of TinkerPop.
newtype GTraversal s e = GTraversal { unGTraversal :: Gremlin }
                       deriving (Show)

-- | 'id' is @__.identity()@. '(.)' compose 'GTraversal's by
-- @.flatMap@ step.
instance Category GTraversal where
  id = toGTraversal identity
  a . b = b @. flatMap a

-- | Unsafely convert output type.
instance Functor (GTraversal s) where
  fmap _ = GTraversal . unGTraversal

-- | Something that is isomorphic to 'Gremlin'.
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

unsafeGTraversal :: Gremlin -> GTraversal s e
unsafeGTraversal = GTraversal

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

-- | Unsafely convert output type
instance Functor (GStep s) where
  fmap _ = GStep . unGStep

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

unsafeGStep :: Gremlin -> GStep s e
unsafeGStep = GStep

-- | @.identity@ step.
identity :: GStep s s
identity = unsafeFromGremlin ".identity()"

-- | @.filter@ step with lambda block.
filterL :: Gremlin
          -- ^ Gremlin code inside filter's @{}@ block.
        -> GStep s s
filterL block = unsafeFromGremlin (".filter({" <> block <> "})")

-- | @.filter@ step with steps(traversal).
filter :: ToGTraversal g => g s e -> GStep s s
filter step = unsafeFromGremlin (".filter(" <> (toGremlin $ toGTraversal step) <> ")")

unsafeFilterStep :: (s -> a) -> Gremlin -> GStep s s
unsafeFilterStep _ = unsafeGStep

-- | @.has@ step.
has :: Element s
    => Gremlin -- ^ target
    -> Gremlin -- ^ expectation
    -> GStep s s
has target expec = unsafeFilterStep (getPropertyValue target) (".has(" <> target <> ", " <> expec <> ")")

genericMultiArgFilter :: Gremlin -- ^ method name
                      -> (s -> a) -- ^ phantom filtering accessor
                      -> [Gremlin] -- ^ arguments
                      -> GStep s s
genericMultiArgFilter method_name f args = unsafeFilterStep f ("." <> method_name <> "(" <> args_g <> ")")
  where
    args_g = T.intercalate ", " args

-- | @.hasLabel@ step
hasLabel :: Element s
         => [Gremlin] -- ^ expected label names
         -> GStep s s
hasLabel = genericMultiArgFilter "hasLabel" getLabel

hasId :: Element s
      => [Gremlin] -- ^ expected IDs
      -> GStep s s
hasId = genericMultiArgFilter "hasId" getElementID

-- | @.or@ step.
or :: ToGTraversal g => [g s e] -> GStep s s
or conds = unsafeFromGremlin (".or(" <> conds_g <> ")")
  where
    conds_g = T.intercalate ", " $ map toG conds
    toG cond = toGremlin $ toGTraversal cond

-- | @.not@ step.
not :: ToGTraversal g => g s e -> GStep s s
not cond = unsafeFromGremlin (".not(" <> (toGremlin $ toGTraversal cond) <> ")")

-- | @.range@ step.
range :: Gremlin
      -- ^ min
      -> Gremlin
      -- ^ max
      -> GStep s s
range min_g max_g = unsafeFromGremlin (".range(" <> min_g <> ", " <> max_g <> ")")

-- | @.order@ and @.by@ steps
orderBy :: ToGTraversal g
        => [(g s e, Gremlin)]
           -- ^ (accessor steps, comparator) of each @.by@
        -> GStep s s
orderBy bys = unsafeFromGremlin (".order()" <> bys_g)
  where
    bys_g = mconcat $ map toG bys
    toG (accessor, comparator) =
      ".by(" <> (toGremlin $ toGTraversal accessor) <> ", " <> comparator <> ")"

-- | @.flatMap@ step
flatMap :: ToGTraversal g => g s e -> GStep s e
flatMap gt = unsafeFromGremlin (".flatMap(" <> (toGremlin $ toGTraversal gt) <> ")")

unsafeTransformStep :: (a -> b) -> Gremlin -> GStep a b
unsafeTransformStep _ = unsafeGStep

-- | @.values@ step.
values :: Element s
       => [Gremlin]
       -- ^ property keys
       -> GStep s PropertyValue
values keys = unsafeTransformStep (getPropertyValue "DUMMY") (".values(" <> keys_g <> ")")
  where
    keys_g = T.intercalate ", " keys

genericTraversalStep :: Gremlin -> [Gremlin] -> GStep Vertex e
genericTraversalStep method_name edge_labels =
  unsafeFromGremlin ("." <> method_name <> "(" <> labels_g <> ")")
  where
    labels_g = T.intercalate ", " edge_labels

-- | @.out@ step
out :: [Gremlin] -- ^ edge labels
    -> GStep Vertex Vertex
out = genericTraversalStep "out"

-- | @.outE@ step
outE :: [Gremlin] -- ^ edge labels
     -> GStep Vertex Edge
outE = genericTraversalStep "outE"

-- | @.in@ step (@in@ is reserved by Haskell..)
inS :: [Gremlin] -- ^ edge labels
    -> GStep Vertex Vertex
inS = genericTraversalStep "in"

inE :: [Gremlin] -- ^ edge labels
    -> GStep Vertex Edge
inE = genericTraversalStep "inE"
