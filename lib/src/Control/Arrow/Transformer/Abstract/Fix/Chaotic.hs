{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Transformer.Abstract.Fix.Chaotic where

import           Prelude hiding (pred,lookup,map,head,iterate,(.))

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Fix
import           Control.Arrow.Reader
import           Control.Arrow.Trans
import           Control.Arrow.Order(ArrowComplete(..),ArrowJoin(..),ArrowEffectCommutative)
import           Control.Arrow.Transformer.Reader
import           Control.Arrow.Transformer.State
import           Control.Arrow.Transformer.Writer

import           Data.Profunctor
import           Data.Order
import           Data.HashSet(HashSet)
import qualified Data.HashSet as H
import           Data.Identifiable
import           Data.Empty
import           Data.Coerce

import           Data.Abstract.Cache (IsCache)
import qualified Data.Abstract.Cache as Cache
import           Data.Abstract.StackWidening(Stack(..))
import           Data.Abstract.Widening(Stable(..))

newtype ChaoticT cache a b c x y =
  ChaoticT (
    WriterT (Component a)
      (StateT (cache a b)
        (ReaderT (Stack a) c)) x y)
  deriving (Profunctor,Category,Arrow,ArrowChoice)

runChaoticT :: (IsCache cache a b, Profunctor c) => ChaoticT cache a b c x y -> c x (cache a b,y)
runChaoticT (ChaoticT f) = dimap (\a -> (empty,(empty,a))) (second snd) (runReaderT (runStateT (runWriterT f)))
{-# INLINE runChaoticT #-}

runChaoticT' :: (IsCache cache a b, Profunctor c) => ChaoticT cache a b c x y -> c x y
runChaoticT' f = rmap snd (runChaoticT f)
{-# INLINE runChaoticT' #-}

chaotic :: (Identifiable a, IsCache cache a b, Profunctor c, ArrowChoice c, ArrowApply c) => Cache.Widening cache a b -> IterationStrategy (ChaoticT cache a b c) a b
chaotic widen (ChaoticT (WriterT (StateT f))) = ChaoticT $ WriterT $ StateT $ push $ proc (stack,cache,a) -> do
  case Cache.lookup a cache of
    -- If the cache contains a stable entry, just return it.
    Just (Stable,b) ->
      returnA -< (cache,(mempty,b))

    -- If the entry has appeared on the stack, stop recursion and
    -- return the cached entry. Remember with the fixpoint component
    -- set that we need to iterate on this entry.
    Just (Instable,b) | H.member a stack ->
      returnA -< (cache,(Component {head = H.singleton a, body = H.empty},b))

    -- If we did not encounter the entry, register the entry and keep
    -- recursing.
    _ ->
      iterate -<< (Cache.initialize a cache,a)

  where
    iterate = proc (cache,a) -> do
      (cache',(component,b)) <- f -< (cache,a)

      case () of
        -- The call did not depend on any unstable calls. This means
        -- we are done and don't need to iterate.
        () | H.null (head component) -> do
             returnA -< (Cache.insert a b Stable cache',(mempty,b))

        -- We are at the head of a fixpoint component. This means, we
        -- have to iterate until the head stabilized.
           | head component == H.singleton a -> do
             let ((stable,bNew),cache'') = Cache.update widen a b cache'

             case stable of
               -- If the head of a fixpoint component is stable, set
               -- all elements in the body of the component as stable
               -- too and return.
               Stable -> do
                 returnA -< (foldr Cache.setStable cache'' (body component),(mempty,bNew))

               -- If the head of a fixpoint component is not stable, keep iterating.
               Instable ->
                 iterate -<< (cache'',a)

        -- We are inside an  fixpoint component, but its head has not stabilized.
           | otherwise -> do
             returnA -< (Cache.insert a b Instable cache',
                          (Component { head = H.delete a (head component),
                                       body = H.insert a (body component) }, b)) 

    push g = proc (cache,a) -> do
      Stack xs <- ask -< ()
      local g -< (Stack (H.insert a xs),(xs,cache,a))

instance (Identifiable a,IsCache cache a b, ArrowRun c) => ArrowRun (ChaoticT cache a b c) where
  type Rep (ChaoticT cache a b c) x y = Rep c x (cache a b,y)
  run = run . runChaoticT
  {-# INLINE run #-}

instance (Identifiable a,Profunctor c,ArrowApply c) => ArrowApply (ChaoticT cache a b c) where
  app = ChaoticT (lmap (first coerce) app)
  {-# INLINE app #-}

instance (Identifiable a,Profunctor c,Arrow c) => ArrowJoin (ChaoticT cache a b c) where
  joinSecond g = second g
  {-# INLINE joinSecond #-}

instance (Identifiable a,Profunctor c,Arrow c, Complete y) => ArrowComplete y (ChaoticT cache a b c) where
  ChaoticT f <⊔> ChaoticT g = ChaoticT $ rmap (uncurry (⊔)) (f &&& g)
  {-# INLINE (<⊔>) #-}

data Component a = Component { head :: HashSet a, body :: HashSet a }
instance Identifiable a => Semigroup (Component a) where (<>) = mappend
instance Identifiable a => Monoid (Component a) where
  mempty = Component { head = H.empty, body = H.empty }
  c1 `mappend` c2 = Component { head = head c1 <> head c2, body = body c1 <> body c2 }
  {-# INLINE mempty #-}
  {-# INLINE mappend #-}

instance (Identifiable a, ArrowEffectCommutative c) => ArrowEffectCommutative (ChaoticT cache a b c) where
