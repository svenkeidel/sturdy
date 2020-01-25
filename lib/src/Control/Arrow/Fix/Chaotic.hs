{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FunctionalDependencies #-}
module Control.Arrow.Fix.Chaotic where

import           Prelude hiding (head,iterate,map)

import           Control.Arrow
import           Control.Arrow.Trans
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

import           Text.Printf

class (Arrow c, Profunctor c) => ArrowChaotic a c | c -> a where
  setComponent :: c (Component a,y) y
  getComponent :: c x y -> c x (Component a,y)

  default setComponent :: (c ~ t c', ArrowLift t, ArrowChaotic a c') => c (Component a,y) y
  setComponent = lift' setComponent
  {-# INLINE setComponent #-}

chaotic :: forall a b c. (ArrowStack a c, ArrowCache a b c, ArrowChoice c) => FixpointCombinator c a b
chaotic f = proc a -> do
  m <- Cache.lookup -< a
  case m of
    Just (_,b) -> returnA -< b
    Nothing    -> do
      Cache.initialize -< a
      iterate -< a
  where
    iterate = proc a -> do
      b <- Stack.push f -< a
      (stable,b') <- Cache.update -< (a,b)
      case stable of
        Stable   -> returnA -< b'
        Unstable -> iterate -< a
{-# INLINE chaotic #-}

-- | Iterate on the innermost fixpoint component.
iterateInner :: forall a b c. (Identifiable a, ArrowChaotic a c, ArrowStack a c, ArrowCache a b c, ArrowChoice c) => FixpointCombinator c a b
{-# INLINE iterateInner #-}
iterateInner f = proc a -> do
  m <- Cache.lookup -< a
  case m of
    Just (Stable,b)   -> returnA -< b
    Just (Unstable,b) -> setComponent -< (mempty { head = H.singleton a }, b)
    Nothing -> do
      Cache.initialize -< a
      iterate -< a
  where
    iterate = proc a -> do
      (component,b) <- getComponent (Stack.push f) -< a

      -- The call did not depend on any unstable calls. This means
      -- we are done and don't need to iterate.
      if H.null (head component)
      then do
        Cache.write -< (a,b,Stable)
        returnA -< b
      else do
        (stable,bNew) <- Cache.update -< (a,b)
        case stable of
          Stable ->
            if head component == H.singleton a
            then do
              map Cache.setStable -< (Stable,) <$> H.toList (body component)
              returnA -< bNew
            else do
              Cache.setStable -< (Unstable,a)
              setComponent -< (component { head = H.delete a (head component)
                                         , body = H.insert a (body component) }, bNew)
          Unstable -> iterate -< a

-- | Iterate on the outermost fixpoint component.
iterateOuter :: forall a b c. (Identifiable a, ArrowChaotic a c, ArrowStack a c, ArrowCache a b c, ArrowChoice c) => FixpointCombinator c a b
{-# INLINE iterateOuter #-}
iterateOuter f = proc a -> do
  m <- Cache.lookup -< a
  case m of
    Just (Stable,b)   -> returnA -< b
    Just (Unstable,b) -> setComponent -< (mempty { head = H.singleton a }, b)
    Nothing -> do
      Cache.initialize -< a
      iterate -< a
  where
    iterate = proc a -> do
      (component,b) <- getComponent (Stack.push f) -< a
      case () of
        -- The call did not depend on any unstable calls. This means
        -- we are done and don't need to iterate.
        () | H.null (head component) -> do
             Cache.write -< (a,b,Stable)
             returnA -< b

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
                 returnA -< bNew

               -- If the head of a fixpoint component is not stable, keep iterating.
               Unstable ->
                 iterate -< a

        -- We are inside an  fixpoint component, but its head has not stabilized.
           | otherwise -> do
             Cache.write -< (a,b,Unstable)
             setComponent -< (component { head = H.delete a (head component)
                                        , body = H.insert a (body component) }, b)

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

instance Identifiable a => Monoid (Component a) where
  mempty = Component { head = H.empty, body = H.empty }
  mappend = (<>)
  {-# INLINE mempty #-}
  {-# INLINE mappend #-}

singleton :: Identifiable a => a -> Component a
singleton a = Component { head = H.singleton a, body = H.empty }
{-# INLINE singleton #-}

instance Show a => Show (Component a) where
  show (Component h b) = printf "Component { head = %s, body = %s }" (show (H.toList h)) (show (H.toList b))
