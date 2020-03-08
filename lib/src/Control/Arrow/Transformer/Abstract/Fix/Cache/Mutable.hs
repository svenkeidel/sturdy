{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Transformer.Abstract.Fix.Cache.Mutable where

import           Prelude hiding ((.))

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Fix.Cache as Cache
import           Control.Arrow.Order hiding (bottom)
import           Control.Arrow.Primitive
import           Control.Arrow.Trans
import           Control.Arrow.Transformer.Const
import           Control.Arrow.Transformer.Abstract.Fix.Cache.Immutable (Widening)

import           Data.Abstract.Stable
import qualified Data.Abstract.Widening as W

import           Data.Coerce
import           Data.Identifiable
import           Data.Monoidal
import           Data.Order
import           Data.Profunctor
import           Data.Profunctor.Unsafe
import           Data.HashTable (HashTable)
import qualified Data.HashTable as Map

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

class NewCache cache a b where
  newCache :: ArrowPrimitive c => c () (cache c a b)

----- Basic Cache -----
newtype Cache c a b = Cache (HashTable (PrimState c) a (Stable,b))

type instance Widening (Cache c a b) = W.Widening b

instance NewCache Cache a b where
  newCache = rmap Cache Map.new

instance (Identifiable a, LowerBounded b, ArrowChoice c, ArrowPrimitive c)
  => ArrowCache a b (CacheT Cache a b c) where
  initialize = lift $ \(_,Cache cache) -> proc a -> do
    (_,b) <- Map.initialize -< (a,(Unstable,bottom),cache)
    returnA -< b
  lookup = lift $ \(_,Cache cache) -> proc a ->
    Map.lookup -< (a,cache)
  update = lift $ \(widen,Cache cache) -> proc (a,b) -> do
    m <- Map.lookup -< (a,cache)
    case m of
      Just (Stable,b') ->
        returnA -< (Stable,a,b')
      Just (Unstable,b') -> do
        let (st,b'') = widen b' b
        Map.insert -< (a,(st,b''),cache)
        returnA -< (st,a,b'')
      Nothing -> do
        Map.insert -< (a,(Unstable,b),cache)
        returnA -< (Unstable,a,b)
  write = lift $ \(_,Cache cache) -> proc (a,b,s) ->
    Map.insert -< (a,(s,b),cache)
  setStable = lift $ \(_,Cache cache) -> proc (s,a) ->
    Map.update (\_ s m -> (fmap (first (const s)) m,())) -< (a,s,cache)
  {-# INLINE initialize #-}
  {-# INLINE lookup #-}
  {-# INLINE write #-}
  {-# INLINE update #-}
  {-# INLINE setStable #-}

------ Group Cache ------
data Group cache c a b where
  Groups :: HashTable (PrimState c) k (cache c a b) -> Group cache c (k,a) b

type instance Widening (Group cache c (k,a) b) = Widening (cache c a b)

instance NewCache (Group cache) (k,a) b where
  newCache = rmap Groups Map.new

instance (Identifiable k, NewCache cache a b, ArrowChoice c, ArrowApply c, ArrowCache a b (CacheT cache a b c), ArrowPrimitive c)
  => ArrowCache (k,a) b (CacheT (Group cache) (k,a) b c) where
  initialize = withGroup Cache.initialize
  lookup = withGroup Cache.lookup
  update = proc ((k,a),b) -> do
    (st,a',b') <- withGroup Cache.update -< (k,(a,b))
    returnA -< (st,(k,a'),b')
  write = lmap (\((k,a),b,s) -> (k,(a,b,s))) (withGroup Cache.write)
  setStable = lmap shuffle1 (withGroup Cache.setStable)
  {-# INLINE initialize #-}
  {-# INLINE lookup #-}
  {-# INLINE write #-}
  {-# INLINE update #-}
  {-# INLINE setStable #-}

withGroup :: (Identifiable k, NewCache cache a b, ArrowChoice c, ArrowApply c, ArrowPrimitive c)
  => CacheT cache a b c x y -> CacheT (Group cache) (k,a) b c (k,x) y
withGroup f = lift $ \(widen,Groups groups) -> proc (k,a) -> do
  m <- Map.lookup -< (k,groups)
  cache <- case m of
    Just cache -> returnA -< cache
    Nothing -> do
      cache <- newCache -< ()
      Map.insert -< (k,cache,groups)
      returnA -< cache
  unlift f (widen,cache) -<< a
{-# INLINE withGroup #-}
