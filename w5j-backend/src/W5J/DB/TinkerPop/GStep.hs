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
         GScriptLike(..),
         ToGTraversal(..),
         -- ** Step markers,
         Filter,
         General,
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

-- TODO: ダミーのクラスメソッドって、本当に必要？

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
instance Category (GStep c) where
  id = forgetFilter gIdentity
  bc . ab = unsafeFromGScript (unGStep ab <> unGStep bc)

-- | Unsafely convert output type
instance Functor (GStep c s) where
  fmap _ = GStep . unGStep

-- | Call static method versions of the 'GStep' on @__@ class.
instance ToGTraversal GStep where
  toGTraversal step = unsafeFromGScript (gRaw "__" <> toGScript step)
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
instance Category (GTraversal c) where
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
  toGTraversal :: g c s e -> GTraversal c s e
  forgetFilter :: g Filter s e -> g c s e
  -- ^ Treat a filtering step/traversal as any type of step. Use this
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
data General

unsafeGTraversal :: GScript -> GTraversal c s e
unsafeGTraversal = GTraversal

allVertices :: GTraversal General Void Vertex
allVertices = unsafeFromGScript $ gRaw "g.V()"

vertexByID :: GScript
              -- ^ Gremlin code for vertex ID.
           -> GTraversal General Void Vertex
vertexByID vid = unsafeFromGScript (gRaw "g" <> gMethodCall "V" [vid])

infixl 5 @.

(@.) :: GTraversal c a b -> GStep c b d -> GTraversal c a d
gt @. gs = unsafeFromGScript (toGScript gt <> toGScript gs)


-- | Vertex in a TinkerPop graph.
data Vertex

-- | Edge in a TinkerPop graph.
data Edge

-- | Element interface in a TinkerPop graph.
class Element e where
  getPropertyValue :: GScript -> e -> PropertyValue
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

unsafeGStep :: GScript -> GStep c s e
unsafeGStep = GStep

-- | @.identity@ step.
gIdentity :: GStep Filter s s
gIdentity = unsafeFromGScript $ gMethodCall "identity" []

-- | @.filter@ step with lambda block.
gFilterL :: GScript
         -- ^ Gremlin code inside filter's @{}@ block.
         -> GStep Filter s s
gFilterL block = unsafeFromGScript (gMethodCall "filter" [gRaw "{" <> block <> gRaw "}"])

-- | @.filter@ step with steps(traversal).
gFilter :: ToGTraversal g => g c s e -> GStep Filter s s
gFilter step = unsafeFromGScript (gMethodCall "filter" [toGScript $ toGTraversal step])

unsafeFilterStep :: (s -> a) -> GScript -> GStep Filter s s
unsafeFilterStep _ = unsafeGStep

-- | @.has@ step.
gHas :: Element s
     => GScript -- ^ target
     -> GScript -- ^ expectation
     -> GStep Filter s s
gHas target expec = unsafeFilterStep (getPropertyValue target)
                    (gMethodCall "has" [target, expec])

genericMultiArgFilter :: Text -- ^ method name
                      -> (s -> a) -- ^ phantom filtering accessor
                      -> [GScript] -- ^ arguments
                      -> GStep Filter s s
genericMultiArgFilter method_name f args =
  unsafeFilterStep f $ gMethodCall method_name args

-- | @.hasLabel@ step
gHasLabel :: Element s
          => [GScript] -- ^ expected label names
          -> GStep Filter s s
gHasLabel = genericMultiArgFilter "hasLabel" getLabel

gHasId :: Element s
       => [GScript] -- ^ expected IDs
       -> GStep Filter s s
gHasId = genericMultiArgFilter "hasId" getElementID

-- | @.or@ step.
gOr :: ToGTraversal g => [g c s e] -> GStep Filter s s
gOr conds = unsafeFromGScript (gMethodCall "or" $ map toG conds)
  where
    toG cond = toGScript $ toGTraversal cond

-- | @.not@ step.
gNot :: ToGTraversal g => g c s e -> GStep Filter s s
gNot cond = unsafeFromGScript (gMethodCall "not" [toGScript $ toGTraversal cond])

-- | @.range@ step.
gRange :: GScript
       -- ^ min
       -> GScript
       -- ^ max
       -> GStep Filter s s
gRange min_g max_g = unsafeFromGScript (gMethodCall "range" [min_g, max_g])

-- | @.order@ and @.by@ steps
gOrderBy :: ToGTraversal g
         => [(g c s e, GScript)]
         -- ^ (accessor steps, comparator) of each @.by@
         -> GStep Filter s s
gOrderBy bys = unsafeFromGScript (gMethodCall "order" [] <> bys_g)
  where
    bys_g = mconcat $ map toG bys
    toG (accessor, comparator) =
      gMethodCall "by" [(toGScript $ toGTraversal accessor), comparator]

-- | @.flatMap@ step
gFlatMap :: ToGTraversal g => g c s e -> GStep c s e
gFlatMap gt = unsafeFromGScript (gMethodCall "flatMap" [toGScript $ toGTraversal gt])

unsafeTransformStep :: (a -> b) -> GScript -> GStep General a b
unsafeTransformStep _ = unsafeGStep

-- | @.values@ step.
gValues :: Element s
        => [GScript]
        -- ^ property keys
        -> GStep General s PropertyValue
gValues keys = unsafeTransformStep (getPropertyValue "DUMMY") (gMethodCall "values" keys)

genericTraversalStep :: Text -> [GScript] -> GStep General Vertex e
genericTraversalStep method_name edge_labels =
  unsafeFromGScript (gMethodCall method_name edge_labels)

-- | @.out@ step
gOut :: [GScript] -- ^ edge labels
     -> GStep General Vertex Vertex
gOut = genericTraversalStep "out"

-- | @.outE@ step
gOutE :: [GScript] -- ^ edge labels
      -> GStep General Vertex Edge
gOutE = genericTraversalStep "outE"

-- | @.in@ step (@in@ is reserved by Haskell..)
gIn :: [GScript] -- ^ edge labels
    -> GStep General Vertex Vertex
gIn = genericTraversalStep "in"

gInE :: [GScript] -- ^ edge labels
     -> GStep General Vertex Edge
gInE = genericTraversalStep "inE"

---- -- probably we can implement .as() step like this. GBuilder generates
---- -- some 'Label', which is passed to .as() step and can be passed later
---- -- to .select() step etc.
---- as :: GBuilder (Label, GStep Filter s s)
---- as = undefined
