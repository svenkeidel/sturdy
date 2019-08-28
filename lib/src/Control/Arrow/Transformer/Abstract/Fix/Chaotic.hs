{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Control.Arrow.Transformer.Abstract.Fix.Chaotic(ChaoticT,runChaoticT,iterateOuter,iterateInner) where

import           Prelude hiding (pred,lookup,map,head,iterate,(.),elem)

import           Control.Category
import           Control.Arrow hiding (loop)
import           Control.Arrow.Fix
import           Control.Arrow.Cache as Cache
import           Control.Arrow.Trans
import           Control.Arrow.Order(ArrowComplete(..),ArrowJoin(..),ArrowEffectCommutative)
import           Control.Arrow.Utils

import           Control.Arrow.Transformer.Writer

import           Data.Profunctor
import           Data.Order
import           Data.HashSet(HashSet)
import qualified Data.HashSet as H
import           Data.Identifiable
import           Data.Coerce

import           Data.Abstract.Widening(Stable(..))
import           Text.Printf

newtype ChaoticT a b c x y = ChaoticT (WriterT (Component a) c x y)
  deriving (Profunctor,Category,Arrow,ArrowChoice)

type instance Fix (ChaoticT _ _ c) x y = ChaoticT x y c
instance (Identifiable a, ArrowCacheRecurse a b c, ArrowChoice c) => ArrowFix (ChaoticT a b c a b) where
  fix f = iterateInner (f (fix f))
  {-# INLINABLE fix #-}

-- | Iterate on the innermost fixpoint component.
iterateInner :: (Identifiable a, ArrowCacheRecurse a b c, ArrowChoice c) => IterationStrategy (ChaoticT a b c) a b
{-# INLINE iterateInner #-}
iterateInner f = lift $ recurse $ proc (a,r) -> do
  case r of
    Cached (Stable,b) -> returnA -< (mempty,b)
    Cached (Instable,b) -> returnA -< (Component {head = H.singleton a, body = H.empty},b)
    Compute -> iterate -< a
  where
    iterate = proc a -> do
      (component,b) <- unlift f -< a
      if H.null (head component)
      then do
        Cache.write -< (a,b,Stable)
        returnA -< (mempty,b)
      else do
        (stable,bNew) <- Cache.update -< (a,b)
        case stable of
          Stable   -> returnA -< (component { head = H.delete a (head component) },bNew)
          Instable -> iterate -< a

-- | Iterate on the outermost fixpoint component.
iterateOuter :: (Identifiable a, ArrowCacheRecurse a b c, ArrowChoice c) => IterationStrategy (ChaoticT a b c) a b
{-# INLINE iterateOuter #-}
iterateOuter f = lift $ recurse $ proc (a,r) -> case r of
  -- If the cache contains a stable entry, just return it.
  Cached (Stable,b) -> returnA -< (mempty,b)

  -- If the cache contains an unstable entry, remember to iterate on this entry.
  Cached (Instable,b) -> returnA -< (Component {head = H.singleton a, body = H.empty},b)

  -- If we did not encounter the entry, register the entry and keep recursing.
  Compute -> iterate -< a

  where
    iterate = proc a -> do
      (component,b) <- unlift f -< a

      case () of
        -- The call did not depend on any unstable calls. This means
        -- we are done and don't need to iterate.
        () | H.null (head component) -> do
             Cache.write -< (a,b,Stable)
             returnA -< (mempty,b)

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
                 returnA -< (mempty,bNew)

               -- If the head of a fixpoint component is not stable, keep iterating.
               Instable ->
                 iterate -< a

        -- We are inside an  fixpoint component, but its head has not stabilized.
           | otherwise -> do
             Cache.write -< (a,b,Instable)
             returnA -< (Component { head = H.delete a (head component),
                                     body = H.insert a (body component) }, b)

runChaoticT :: Profunctor c => ChaoticT a b c x y -> c x y
runChaoticT (ChaoticT f) = rmap snd (runWriterT f)
{-# INLINE runChaoticT #-}

instance (Identifiable a, ArrowRun c) => ArrowRun (ChaoticT a b c) where
  type Run (ChaoticT a b c) x y = Run c x y
  run f = run (runChaoticT f)
  {-# INLINE run #-}

instance ArrowTrans (ChaoticT a b c) where
  type Underlying (ChaoticT a b c) x y = c x (Component a,y)

instance (Identifiable a, Profunctor c,ArrowApply c) => ArrowApply (ChaoticT a b c) where
  app = ChaoticT (lmap (first coerce) app)
  {-# INLINE app #-}

instance (Identifiable a, Profunctor c,Arrow c) => ArrowJoin (ChaoticT a b c) where
  joinSecond g = second g
  {-# INLINE joinSecond #-}

instance (Identifiable a,Profunctor c,Arrow c, Complete y, ArrowEffectCommutative c) => ArrowComplete y (ChaoticT a b c) where
  ChaoticT f <⊔> ChaoticT g = ChaoticT $ rmap (uncurry (⊔)) (f &&& g)
  {-# INLINE (<⊔>) #-}

data Component a = Component { head :: HashSet a, body :: HashSet a } deriving (Eq)
instance Identifiable a => Semigroup (Component a) where
  Component h1 b1 <> Component h2 b2 = Component { head = h1 <> h2, body = b1 <> b2 }
  {-# INLINE (<>) #-}
instance Identifiable a => Monoid (Component a) where
  mempty = Component { head = H.empty, body = H.empty }
  mappend = (<>)
  {-# INLINE mempty #-}
  {-# INLINE mappend #-}
instance Show a => Show (Component a) where
  show (Component h b) = printf "Component { head = %s, body = %s }" (show (H.toList h)) (show (H.toList b))

instance (Identifiable a, ArrowEffectCommutative c) => ArrowEffectCommutative (ChaoticT a b c) where
