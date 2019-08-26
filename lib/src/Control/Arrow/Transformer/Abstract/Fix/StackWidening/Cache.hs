{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Transformer.Abstract.Fix.StackWidening.Cache where

import Prelude hiding (pred,lookup,map,head,iterate,(.))

import Control.Category
import Control.Arrow
import Control.Arrow.Cache
import Control.Arrow.Const
import Control.Arrow.State
import Control.Arrow.Trans
import Control.Arrow.Order(ArrowJoin(..),ArrowComplete(..),ArrowEffectCommutative)

import Control.Arrow.Transformer.Const
import Control.Arrow.Transformer.State

import Data.Profunctor
import Data.Profunctor.Unsafe((.#))
import Data.Identifiable
import Data.Coerce
import Data.Empty
import Data.Order
import Data.HashMap.Lazy(HashMap)
import qualified Data.HashMap.Lazy as M

import Data.Abstract.Widening(Widening,Stable(..))
import Data.Maybe(fromMaybe)

newtype CacheT a b c x y = CacheT (ConstT (Widening b) (StateT (Cache a b) c) x y)
  deriving (Profunctor,Category,Arrow,ArrowChoice)

newtype Cache a b = Cache { getMap :: HashMap a (Stable,b)}
instance (Show a, Show b) => Show (Cache a b) where
  show (Cache m) = show (M.toList m)

instance IsEmpty (Cache a b) where
  empty = Cache M.empty
  {-# INLINE empty #-}

instance (Identifiable a, LowerBounded b, Arrow c, Profunctor c) => ArrowCache a b (CacheT a b c) where
  lookup = CacheT $ proc a -> do
    Cache cache <- get -< ()
    returnA -< M.lookup a cache
  write = CacheT $ modify' (\((a,b,s),Cache cache) -> ((),Cache (M.insert a (s,b) cache)))
  update = CacheT $ askConst $ \widen -> modify' (\((a,b),Cache cache) ->
    let (_,bOld) = fromMaybe (Instable,bottom) (M.lookup a cache)
        bNew = widen bOld b
    in (bNew,Cache (M.insert a bNew cache)))
  setStable = CacheT $ modify' $ \((s,a),Cache cache) -> ((),Cache (M.adjust (first (const s)) a cache))
  {-# INLINE lookup #-}
  {-# INLINE write #-}
  {-# INLINE update #-}
  {-# INLINE setStable #-}

runCacheT :: (Profunctor c)
          => Widening b -> CacheT a b c x y -> c x (Cache a b, y)
runCacheT widen (CacheT f) = lmap (\x -> (empty,x)) (runStateT (runConstT widen f))
{-# INLINE runCacheT #-}

instance (ArrowRun c) => ArrowRun (CacheT a b c) where
  type Run (CacheT a b c) x y = Widening b -> Run c x (Cache a b,y)
  run f widen = run (runCacheT widen f)
  {-# INLINE run #-}

instance (Profunctor c,ArrowApply c) => ArrowApply (CacheT a b c) where
  app = CacheT (app .# first coerce)
  {-# INLINE app #-}

instance (Complete y, ArrowEffectCommutative c) => ArrowComplete y (CacheT a b c) where
  CacheT f <⊔> CacheT g = CacheT $ rmap (uncurry (⊔)) (f &&& g)
  {-# INLINE (<⊔>) #-}

instance (Arrow c, Profunctor c) => ArrowJoin (CacheT a b c) where
  joinSecond (CacheT f) = CacheT (second f)
  {-# INLINE joinSecond #-}

instance ArrowLift (CacheT a b) where
  lift' f = CacheT (lift' (lift' f))
  {-# INLINE lift' #-}

instance ArrowEffectCommutative c => ArrowEffectCommutative (CacheT a b c)
