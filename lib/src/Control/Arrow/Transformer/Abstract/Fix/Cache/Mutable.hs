{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Arrow.Transformer.Abstract.Fix.Cache.Mutable where

import Prelude hiding ((.))

import Control.Category
import Control.Arrow
import Control.Arrow.Order
import Control.Arrow.Primitive
import Control.Arrow.Trans
import Control.Arrow.Transformer.Const
import Control.Arrow.Transformer.Abstract.Fix.Cache.Immutable (Widening)

import Data.Coerce
import Data.Order
import Data.Profunctor
import Data.Profunctor.Unsafe

newtype CacheT cache a b c x y = CacheT { unCacheT :: ConstT (Widening (cache c a b), cache c a b) c x y}
  deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowPrimitive)

instance ArrowTrans (CacheT cache a b c) where
  type Underlying (CacheT cache a b c) x y = (Widening (cache c a b), cache c a b) -> c x y
  lift = CacheT . lift
  unlift f = unlift (unCacheT f)
  {-# INLINE lift #-}
  {-# INLINE unlift #-}

instance ArrowLift (CacheT cache a b) where
  lift' = CacheT . lift'
  {-# INLINE lift' #-}

instance (ArrowRun c) => ArrowRun (CacheT cache a b c) where
  type Run (CacheT cache a b c) x y = Widening (cache c a b) -> cache c a b -> Run c x y
  run f widen cache = run (unlift f (widen,cache))
  {-# INLINE run #-}

instance (Complete y, ArrowEffectCommutative c) => ArrowComplete y (CacheT cache a b c) where
  CacheT f <⊔> CacheT g = CacheT $ rmap (uncurry (⊔)) (f &&& g)
  {-# INLINE (<⊔>) #-}

instance (Profunctor c, ArrowApply c) => ArrowApply (CacheT cache a b c) where
  app = CacheT (app .# first coerce)
  {-# INLINE app #-}

instance ArrowEffectCommutative c => ArrowEffectCommutative (CacheT cache a b c)

newtype Cache c a b = Cache (HashMap a (Stable,b))
