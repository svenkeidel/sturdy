{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Arrow.Transformer.Abstract.Fix.Cache where

import Prelude hiding (pred,lookup,map,head,iterate,(.),truncate,elem)

import Control.Category
import Control.Arrow
import Control.Arrow.Trans
import Control.Arrow.State
import Control.Arrow.Fix.Context as Context hiding (Widening)
import Control.Arrow.Order(ArrowJoin(..),ArrowComplete(..),ArrowEffectCommutative)

import Control.Arrow.Transformer.Const
import Control.Arrow.Transformer.State

import Data.Profunctor.Unsafe
import Data.Empty
import Data.Order
import Data.Coerce
import Data.Abstract.Widening

newtype CacheT cache a b c x y = CacheT { unCacheT :: ConstT (Widening b) (StateT (cache a b) c) x y}
  deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowTrans,ArrowContext ctx,ArrowState (cache a b))

runCacheT :: (IsEmpty (cache a b), Profunctor c) => Widening b -> CacheT cache a b c x y -> c x (cache a b,y)
runCacheT widen (CacheT f) = lmap (\x -> (empty,x)) (runStateT (runConstT widen f))
{-# INLINE runCacheT #-}

liftCacheT :: Arrow c => CacheT cache' a' b c x y -> CacheT cache a b c (cache' a' b,x) (cache' a' b,y)
liftCacheT (CacheT f) = CacheT (lift $ \widen -> (withStateT (runConstT widen f)))
{-# INLINE liftCacheT #-}

liftCacheT' :: Arrow c => CacheT cache' a' b c x y -> ConstT (Widening b) (StateT (cache a b) c) (cache' a' b,x) (cache' a' b,y)
liftCacheT' = coerce liftCacheT
{-# INLINE liftCacheT' #-}

instance (IsEmpty (cache a b), ArrowRun c) => ArrowRun (CacheT cache a b c) where
  type Run (CacheT cache a b c) x y = Widening b -> Run c x (cache a b,y)
  run f widen = run (runCacheT widen f)
  {-# INLINE run #-}

instance (Complete y, ArrowEffectCommutative c) => ArrowComplete y (CacheT cache a b c) where
  CacheT f <⊔> CacheT g = CacheT $ rmap (uncurry (⊔)) (f &&& g)
  {-# INLINE (<⊔>) #-}

instance (Arrow c, Profunctor c) => ArrowJoin (CacheT cache a b c) where
  joinSecond (CacheT f) = CacheT (second f)
  {-# INLINE joinSecond #-}

instance (Profunctor c,ArrowApply c) => ArrowApply (CacheT cache a b c) where
  app = CacheT (app .# first coerce)
  {-# INLINE app #-}

instance ArrowEffectCommutative c => ArrowEffectCommutative (CacheT cache a b c)
