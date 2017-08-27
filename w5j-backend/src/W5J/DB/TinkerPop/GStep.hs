{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
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
         GScriptLike(..),
         ToGTraversal(..),
         -- ** Step types
         StepType,
         Filter,
         Transform,
         SideEffect,
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
         gIdentity,
         gFilterL,
         gFilter,
         gHas,
         gHasLabel,
         gHasId,
         gOr,
         gNot,
         gRange,
         -- ** Sorting step
         gOrderBy,
         -- ** Transformation step
         gFlatMap,
         gValues,
         -- ** Graph traversal step
         gOut,
         gOutE,
         gIn,
         gInE
       ) where

import Prelude hiding (or, filter, not)
import Control.Category (Category)
-- (below) to import Category methods without conflict with Prelude
import qualified Control.Category as Category
import Data.Monoid ((<>), mconcat)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import W5J.DB.TinkerPop.GScript (GScript, gRaw, gMethodCall)

-- TODO: forgetFilter呼ぶのダルいな。。identityなどはarbitrary型でいいか？

-- TODO: もうちょい分類すると、markerはFilter, Transform, SideEffectが
-- あるだろう。厄介なのは、and stepなんかはchild traversalがTransform
-- なら結果(parent traversal)はFilterになれるということ。これをどう表
-- 現するか？
--
-- type classを使って全パターンinstance化するのでもいい？なんか警告で
-- ない？
--
-- SideEffectのkindは(* -> *)か？つまり(SideEffect Filter),
-- (SideEffect Transform)が考えられそうではある。
--
-- いっそのこと、SideEffectはGTraversalやGStepを包むモナドにするか？そ
-- うすると、child traversalがSideEffectならparentのSideEffectにならざ
-- るを得ない。しかし、依然としてFilterやTransformの型マーカーは必要に
-- なり、ずいぶん型体系がブサイクになる。。



-- | A Gremlin Step (method call) that takes data @s@ from upstream
-- and emits data @e@ to downstream. Type @c@ is a marker to describe
-- the step.
--
-- 'GStep' is a 'Category'. Use its methods to compose them.
--
-- 'GStep' is not an 'Eq', because it's difficult to define true
-- equality between Gremlin method calls. If we define it naively, it
-- might have conflict with 'Category' law.
newtype GStep c s e = GStep { unGStep :: GScript }
                    deriving (Show)

-- | 'id' is 'identity'.
instance StepType c => Category (GStep c) where
  id = forgetFilter gIdentity
  bc . ab = unsafeGStep (unGStep ab <> unGStep bc)

-- | Unsafely convert output type
instance Functor (GStep c s) where
  fmap _ = GStep . unGStep

-- | Call static method versions of the 'GStep' on @__@ class.
instance ToGTraversal GStep where
  toGTraversal step = unsafeGTraversal (gRaw "__" <> toGScript step)
  forgetFilter = GStep . unGStep

instance GScriptLike (GStep c s e) where
  unsafeFromGScript = GStep
  toGScript = unGStep


-- | GraphTraversal class object of TinkerPop.
--
-- 'GTraversal' is practically the same as 'GStep'. 'GTraversal' is a
-- Java-object in Gremlin domain, while 'GStep' is a chain of method
-- calls.
newtype GTraversal c s e = GTraversal { unGTraversal :: GScript }
                         deriving (Show)
                                  
-- | 'id' is @__.identity()@. '(.)' compose 'GTraversal's by
-- @.flatMap@ step.
instance StepType c => Category (GTraversal c) where
  id = toGTraversal $ forgetFilter gIdentity
  a . b = b @. gFlatMap a

-- | Unsafely convert output type.
instance Functor (GTraversal c s) where
  fmap _ = GTraversal . unGTraversal

-- | Something that is isomorphic to 'GScript'.
class GScriptLike g where
  unsafeFromGScript :: GScript -> g
  toGScript :: g -> GScript

instance GScriptLike (GTraversal c s e) where
  unsafeFromGScript = GTraversal
  toGScript = unGTraversal

-- | Types that can convert to 'GTraversal'.
class ToGTraversal g where
  toGTraversal :: StepType c => g c s e -> GTraversal c s e
  forgetFilter :: StepType c => g Filter s e -> g c s e
  -- ^ Treat a filtering step/traversal as any type of step. Use this
  -- for type matching.

instance ToGTraversal GTraversal where
  toGTraversal = id
  forgetFilter = GTraversal . unGTraversal


-- | Phantom type markers to describe the feature fo the
-- step/traversal.
class StepType t

-- | StepType for filtering steps.
--
-- A filtering step is a step that does filtering only. It takes input
-- and outputs some of them without any modification, traversal
-- actions, or side-effects. Modification of orders (like sorting) is
-- included as filtering.
data Filter

instance StepType Filter

-- | StepType for steps that are not filtering steps and without
-- side-effects. This includes transformations, injections and graph
-- traversal actions.
data Transform

instance StepType Transform

-- | StepType modifier for steps that has side-effects.
data SideEffect t

-- Needs FlexibleInstances extension.
instance StepType (SideEffect Filter)
instance StepType (SideEffect Transform)



unsafeGTraversal :: GScript -> GTraversal c s e
unsafeGTraversal = GTraversal

allVertices :: GTraversal Transform Void Vertex
allVertices = unsafeGTraversal $ gRaw "g.V()"

vertexByID :: GScript
              -- ^ Gremlin code for vertex ID.
           -> GTraversal Transform Void Vertex
vertexByID vid = unsafeGTraversal (gRaw "g" <> gMethodCall "V" [vid])

infixl 5 @.

(@.) :: GTraversal c a b -> GStep c b d -> GTraversal c a d
gt @. gs = unsafeGTraversal (toGScript gt <> toGScript gs)


-- | Vertex in a TinkerPop graph.
data Vertex

-- | Edge in a TinkerPop graph.
data Edge

-- | Element interface in a TinkerPop graph.
class Element e 

instance Element Vertex
instance Element Edge

-- | Value object in a TinkerPop graph.
data PropertyValue

-- | ID object type for Elements
data ElementID

unsafeGStep :: StepType c => GScript -> GStep c s e
unsafeGStep = GStep

-- | @.identity@ step.
gIdentity :: GStep Filter s s
gIdentity = unsafeGStep $ gMethodCall "identity" []

-- | @.filter@ step with lambda block.
gFilterL :: GScript
         -- ^ Gremlin code inside filter's @{}@ block.
         -> GStep Filter s s
gFilterL block = unsafeGStep (gMethodCall "filter" [gRaw "{" <> block <> gRaw "}"])

-- | @.filter@ step with steps(traversal).
gFilter :: (ToGTraversal g, StepType c) => g c s e -> GStep Filter s s
gFilter step = unsafeGStep (gMethodCall "filter" [toGScript $ toGTraversal step])
-- TODO: use Logic typeclass.

-- | @.has@ step.
gHas :: (Element s)
     => GScript -- ^ target
     -> GScript -- ^ expectation
     -> GStep Filter s s
gHas target expec = unsafeGStep $ gMethodCall "has" [target, expec]

-- | @.hasLabel@ step
gHasLabel :: Element s
          => [GScript] -- ^ expected label names
          -> GStep Filter s s
gHasLabel = unsafeGStep . gMethodCall "hasLabel"

-- | @.hasId@ step
gHasId :: Element s
       => [GScript] -- ^ expected IDs
       -> GStep Filter s s
gHasId = unsafeGStep . gMethodCall "hasId"

-- | @.or@ step.
gOr :: (ToGTraversal g, StepType c) => [g c s e] -> GStep Filter s s
gOr conds = unsafeGStep (gMethodCall "or" $ map toG conds)
  where
    toG cond = toGScript $ toGTraversal cond
-- TODO: use Logic typeclass

-- | @.not@ step.
gNot :: (ToGTraversal g, StepType c) => g c s e -> GStep Filter s s
gNot cond = unsafeGStep (gMethodCall "not" [toGScript $ toGTraversal cond])
-- TODO: use Logic typeclass

-- | @.range@ step.
gRange :: GScript
       -- ^ min
       -> GScript
       -- ^ max
       -> GStep Filter s s
gRange min_g max_g = unsafeGStep (gMethodCall "range" [min_g, max_g])

-- | @.order@ and @.by@ steps
gOrderBy :: (ToGTraversal g, StepType c)
         => [(g c s e, GScript)]
         -- ^ (accessor steps, comparator) of each @.by@
         -> GStep Filter s s
gOrderBy bys = unsafeGStep (gMethodCall "order" [] <> bys_g)
  where
    bys_g = mconcat $ map toG bys
    toG (accessor, comparator) =
      gMethodCall "by" [(toGScript $ toGTraversal accessor), comparator]

-- | @.flatMap@ step
gFlatMap :: (ToGTraversal g, StepType c) => g c s e -> GStep c s e
gFlatMap gt = unsafeGStep (gMethodCall "flatMap" [toGScript $ toGTraversal gt])

-- | @.values@ step.
gValues :: Element s
        => [GScript]
        -- ^ property keys
        -> GStep Transform s PropertyValue
gValues = unsafeGStep . gMethodCall "values"

genericTraversalStep :: Text -> [GScript] -> GStep Transform Vertex e
genericTraversalStep method_name edge_labels =
  unsafeGStep (gMethodCall method_name edge_labels)

-- | @.out@ step
gOut :: [GScript] -- ^ edge labels
     -> GStep Transform Vertex Vertex
gOut = genericTraversalStep "out"

-- | @.outE@ step
gOutE :: [GScript] -- ^ edge labels
      -> GStep Transform Vertex Edge
gOutE = genericTraversalStep "outE"

-- | @.in@ step (@in@ is reserved by Haskell..)
gIn :: [GScript] -- ^ edge labels
    -> GStep Transform Vertex Vertex
gIn = genericTraversalStep "in"

gInE :: [GScript] -- ^ edge labels
     -> GStep Transform Vertex Edge
gInE = genericTraversalStep "inE"

---- -- probably we can implement .as() step like this. GBuilder generates
---- -- some 'Label', which is passed to .as() step and can be passed later
---- -- to .select() step etc.
---- as :: GBuilder (Label, GStep Filter s s)
---- as = undefined
