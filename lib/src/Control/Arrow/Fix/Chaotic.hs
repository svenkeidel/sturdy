{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FunctionalDependencies #-}
module Control.Arrow.Fix.Chaotic where

import           Prelude hiding (head,iterate,map)

import           Control.Arrow hiding (loop)
import           Control.Arrow.Fix
import           Control.Arrow.Fix.Stack as Stack
import           Control.Arrow.Fix.Cache as Cache
import           Control.Arrow.Utils(map)

import           Data.Abstract.Stable
import           Data.HashSet (HashSet)
import qualified Data.HashSet as H
import           Data.Identifiable
import           Data.Profunctor
import           Data.Order
import           Data.Empty

import           Text.Printf

class (Arrow c, Profunctor c) => ArrowComponent component c | c -> component where
  setComponent :: c x (component,y) -> c x y
  getComponent :: c x y -> c x (component,y)

detectLoop :: (Identifiable a, ArrowComponent (Component a) c, ArrowStack a c, ArrowCache a b c, ArrowChoice c) => c a (Component a,b) -> c a b
detectLoop iterate = setComponent $ proc a -> do
  loop <- Stack.elem -< a
  if loop
  then do
    m <- Cache.lookup -< a
    case m of
      Just (Stable,b) -> returnA -< (empty, b)
      Just (Unstable,b) -> returnA -< (empty { head = H.singleton a }, b)
      Nothing -> do
        b <- Cache.initialize -< a
        returnA -< (mempty { head = H.singleton a }, b)
  else
    iterate -< a
{-# INLINE detectLoop #-}

-- | Iterate on the innermost fixpoint component.
innermost :: forall a b c. (Identifiable a, ArrowChoice c, ArrowComponent (Component a) c, ArrowStack a c, ArrowCache a b c) => FixpointCombinator c a b
{-# INLINE innermost #-}
innermost f = detectLoop (Stack.push iterate)
  where
    iterate = proc a -> do
      (comp,b) <- getComponent f -< a

      -- The call did not depend on any unstable calls. This means
      -- we are done and don't need to iterate.
      case () of
        () | H.null (head comp) -> returnA -< (empty, b)
           | H.member a (head comp) -> do
              (stable,bNew) <- Cache.update -< (a,b)
              case stable of
                Stable -> returnA -< (Component { head = H.delete a (head comp), body = H.insert a (body comp) }, bNew)
                Unstable -> iterate -< a
           | otherwise ->
             returnA -< (comp, b)

-- | Iterate on the outermost fixpoint component.
outermost :: forall a b c. (Identifiable a, ArrowChoice c, ArrowComponent (Component a) c, ArrowStack a c, ArrowCache a b c) => FixpointCombinator c a b
{-# INLINE outermost #-}
outermost f = detectLoop (Stack.push iterate)
  where
    iterate = proc a -> do
      (component,b) <- getComponent f -< a
      case () of
        -- The call did not depend on any unstable calls. This means
        -- we are done and don't need to iterate.
        () | H.null (head component) -> returnA -< (empty, b)

        -- We are at the head of a fixpoint component. This means, we
        -- have to iterate until the head stabilized.
           | head component == H.singleton a -> do
             (stable,bNew) <- Cache.update -< (a,b)

             case stable of
               -- If the head of a fixpoint component is stable, set
               -- all elements in the body of the component as stable
               -- too and return.
               Stable -> do
                 map Cache.setStable -< H.toList $ H.map (Stable,) (body component)
                 returnA -< (empty, bNew)

               -- If the head of a fixpoint component is not stable, keep iterating.
               Unstable -> iterate -< a

        -- We are inside an  fixpoint component, but its head has not stabilized.
           | otherwise -> do
             let comp = Component { head = H.delete a (head component), body = H.insert a (body component) }
             returnA -< (comp, b)

data Component a = Component { head :: HashSet a, body :: HashSet a } deriving (Eq)

instance Identifiable a => PreOrd (Component a) where
  c1 ⊑ c2 = head c1 ⊑ head c2 && body c1 ⊑ body c2
  {-# INLINE (⊑) #-}

instance Identifiable a => Complete (Component a) where
  c1 ⊔ c2 = c1 <> c2
  {-# INLINE (⊔) #-}

instance Identifiable a => Semigroup (Component a) where
  Component h1 b1 <> Component h2 b2 = Component { head = h1 <> h2, body = b1 <> b2 }
  {-# INLINE (<>) #-}

instance IsEmpty (Component a) where
  empty = Component { head = H.empty, body = H.empty }
  {-# INLINE empty #-}

instance Identifiable a => Monoid (Component a) where
  mempty = empty
  mappend = (<>)
  {-# INLINE mempty #-}
  {-# INLINE mappend #-}

singleton :: Identifiable a => a -> Component a
singleton a = Component { head = H.singleton a, body = H.empty }
{-# INLINE singleton #-}

instance Show a => Show (Component a) where
  show (Component h b) = printf "Component { head = %s, body = %s }" (show (H.toList h)) (show (H.toList b))
