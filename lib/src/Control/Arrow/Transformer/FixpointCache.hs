{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Transformer.FixpointCache(CacheArrow,runCacheArrow) where

import           Prelude hiding (id,(.))

import           Control.Arrow hiding (loop)
import           Control.Arrow.Class.Fail (ArrowFail(..))
import           Control.Arrow.Class.Reader
import           Control.Arrow.Class.State
import           Control.Arrow.Class.FixpointCache
import           Control.Arrow.Utils
import           Control.Category

import           Data.Hashable (Hashable)
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as H
import           Data.Maybe
import           Data.Order

newtype CacheArrow i o c x y = CacheArrow (c ((i,o),x) (o,y))

runCacheArrow :: Arrow c => CacheArrow (HashMap a b) (HashMap a b) c x y -> c x y
runCacheArrow (CacheArrow f) = (\x -> ((H.empty,H.empty),x)) ^>> f >>^ snd

liftCache :: Arrow c => c x y -> CacheArrow i o c x y
liftCache f = CacheArrow $ (\((_,o),x) -> (o,x)) ^>> second f

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

instance (Eq x, Hashable x, LowerBounded y, Complete y, Arrow c) => ArrowCache x y (CacheArrow (HashMap x y) (HashMap x y) c) where
  askCache = CacheArrow $ arr $ \((_,o),x) -> (o,H.lookup x o)
  initializeCache = CacheArrow $ arr $ \((i,o),x) -> (H.insert x (fromMaybe bottom (H.lookup x i)) o,())
  updateCache = CacheArrow $ arr $ \((_,o),(x,y)) -> (H.insertWith (⊔) x y o,())
  retireCache (CacheArrow f) = CacheArrow $ (\((_,o),x) -> ((o,H.empty),x)) ^>> f
  reachedFixpoint = CacheArrow $ arr $ \((i,o),()) ->
    let reached = H.keys i == H.keys o && all (\(k,v_o) -> v_o ⊑ (i H.! k)) (H.toList o)
    in (o,reached)
