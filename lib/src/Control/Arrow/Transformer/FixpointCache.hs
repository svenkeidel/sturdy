{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}
module Control.Arrow.Transformer.FixpointCache(CacheArrow,runCacheArrow,liftCache,Cache,empty,insert,lookup,insertWith,(!),keys,toList) where

import           Prelude hiding (id,lookup)

import           Control.Arrow
import           Control.Arrow.Class.Fail (ArrowFail(..))
import           Control.Arrow.Class.Reader
import           Control.Arrow.Class.State
import           Control.Arrow.Class.FixpointCache
import           Control.Arrow.Utils
import           Control.Category

import           Data.Hashable (Hashable)
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as H
import           Data.HashSet (HashSet)
import qualified Data.HashSet as S
import           Data.Maybe
import           Data.Order

newtype CacheArrow a b c x y = CacheArrow (c ((Cache a b,Cache a b),x) (Cache a b,y))

instance Arrow c => Category (CacheArrow i o c) where
  id = liftCache id
  CacheArrow f . CacheArrow g = CacheArrow $ proc ((i,o),x) -> do
    (o',y) <- g -< ((i,o),x)
    f -< ((i,o'),y)

instance Arrow c => Arrow (CacheArrow i o c) where
  arr f = liftCache (arr f)
  first (CacheArrow f) = CacheArrow $ (\((i,o),(x,y)) -> (((i,o),x),y)) ^>> first f >>^ (\((o,x'),y) -> (o,(x',y)))
  second (CacheArrow f) = CacheArrow $ (\((i,o),(x,y)) -> (x,((i,o),y))) ^>> second f >>^ (\(x,(o,y')) -> (o,(x,y')))

instance ArrowChoice c => ArrowChoice (CacheArrow i o c) where
  left (CacheArrow f) = CacheArrow $ (\((i,o),e) -> injectRight (o,injectLeft ((i,o),e))) ^>> left f >>^ eject
  right (CacheArrow f) = CacheArrow $ (\((i,o),e) -> injectRight ((i,o),injectLeft (o,e))) ^>> right f >>^ eject

instance ArrowState s c => ArrowState s (CacheArrow i o c) where
  getA = liftCache getA
  putA = liftCache putA

instance ArrowReader r c => ArrowReader r (CacheArrow i o c) where
  askA = liftCache askA
  localA (CacheArrow f) = CacheArrow $ (\((i,o),(r,x)) -> (r, ((i,o),x))) ^>> localA f

instance ArrowFail e c => ArrowFail e (CacheArrow i o c) where
  failA = liftCache failA

instance (Eq x, Hashable x, LowerBounded y, Complete y, Arrow c) => ArrowCache x y (CacheArrow x y c) where
  askCache = CacheArrow $ arr $ \((_,o),x) -> (o,lookup x o)
  -- transfer cached value from old fixpoint iteration into the new cache
  initializeCache = CacheArrow $ arr $ \((Cache i,Cache o),x) -> (Cache $ H.insert x (fromMaybe bottom (H.lookup x i)) o,())
  updateCache = CacheArrow $ arr $ \((_,o),(x,y)) -> (insertWith (⊔) x y o,())
  retireCache (CacheArrow f) = CacheArrow $ (\((_,o),x) -> ((o,bottom),x)) ^>> f
  -- we reached or overshot the fixpoint if we landed in the reductive set, i.e. if f x ⊑ x
  reachedFixpoint = CacheArrow $ arr $ \((i,o),()) -> (o,o ⊑ i)

newtype Cache a b = Cache (HashMap a b) deriving (Functor,Foldable,Traversable)

instance (Eq a, Hashable a, PreOrd b) => PreOrd (Cache a b) where
  Cache m1 ⊑ Cache m2 = subsetKeys m1 m2 && all (\(k,v1) -> v1 ⊑ (m2 H.! k)) (H.toList m1)
  Cache m1 ≈ Cache m2 = H.keys m1 == H.keys m2 && all (\(k,v_o) -> v_o ⊑ (m2 H.! k)) (H.toList m1)

instance (Eq a, Hashable a, Complete b) => Complete (Cache a b) where
  Cache m1 ⊔ Cache m2 = Cache (H.unionWith (⊔) m1 m2)

instance (Eq a, Hashable a, CoComplete b) => CoComplete (Cache a b) where
  Cache m1 ⊓ Cache m2 = Cache (H.intersectionWith (⊓) m1 m2)

instance (Eq a, Hashable a, PreOrd b) => LowerBounded (Cache a b) where
  bottom = empty

subsetKeys :: (Eq a, Hashable a) => HashMap a b -> HashMap a b -> Bool
subsetKeys m1 m2 = subset (S.fromMap (H.map (const ()) m1)) (S.fromMap (H.map (const ()) m2))

subset :: (Eq a, Hashable a) => HashSet a -> HashSet a -> Bool
subset s1 s2 = S.size (S.intersection s1 s2) == S.size s1

empty :: Cache a b
empty = Cache H.empty

runCacheArrow :: Arrow c => CacheArrow a b c x y -> Cache a b -> c x y
runCacheArrow (CacheArrow f) i = (\x -> ((i,empty),x)) ^>> f >>^ snd

liftCache :: Arrow c => c x y -> CacheArrow i o c x y
liftCache f = CacheArrow $ (\((_,o),x) -> (o,x)) ^>> second f

lookup :: (Eq a, Hashable a) => a -> Cache a b -> Maybe b
lookup a (Cache m) = H.lookup a m

insert :: (Eq a, Hashable a) => a -> b -> Cache a b -> Cache a b
insert a b (Cache m) = Cache (H.insert a b m)

insertWith :: (Eq a, Hashable a) => (b -> b -> b) -> a -> b -> Cache a b -> Cache a b
insertWith f a b (Cache m) = Cache (H.insertWith f a b m)

(!) :: (Eq a, Hashable a) => Cache a b -> a -> b
Cache m ! a = m H.! a

keys :: Cache a b -> [a]
keys (Cache m) = H.keys m

toList :: Cache a b -> [(a,b)]
toList (Cache m) = H.toList m
