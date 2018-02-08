{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MonoLocalBinds #-}
module Control.Arrow.Transformer.FixpointCache(CacheArrow,runCacheArrow,runCacheArrow',liftCache) where

import           Prelude hiding (id,(.),lookup)
import           Data.Function (fix)

import           Control.Arrow
import           Control.Arrow.Class.Fix
import           Control.Arrow.Utils
import           Control.Category

import           Data.Hashable (Hashable)
import           Data.Maybe
import           Data.Order
import           Data.Store (Store)
import qualified Data.Store as S

newtype CacheArrow a b x y = CacheArrow (((Store a b,Store a b),x) -> (Store a b,y))

runCacheArrow :: CacheArrow a b x y -> (x -> y)
runCacheArrow f = runCacheArrow' f >>^ snd

runCacheArrow' :: CacheArrow a b x y -> (x -> (Store a b,y))
runCacheArrow' (CacheArrow f) = (\x -> ((S.empty,S.empty),x)) ^>> f

liftCache :: (x -> y) -> CacheArrow a b x y
liftCache f = CacheArrow ((\((_,o),x) -> (o,x)) ^>> second f)

instance Category (CacheArrow i o) where
  id = liftCache id
  CacheArrow f . CacheArrow g = CacheArrow $ proc ((i,o),x) -> do
    (o',y) <- g -< ((i,o),x)
    f -< ((i,o'),y)

instance Arrow (CacheArrow i o) where
  arr f = liftCache (arr f)
  first (CacheArrow f) = CacheArrow $ (\((i,o),(x,y)) -> (((i,o),x),y)) ^>> first f >>^ (\((o,x'),y) -> (o,(x',y)))
  second (CacheArrow f) = CacheArrow $ (\((i,o),(x,y)) -> (x,((i,o),y))) ^>> second f >>^ (\(x,(o,y')) -> (o,(x,y')))

instance ArrowChoice (CacheArrow i o) where
  left (CacheArrow f) = CacheArrow $ (\((i,o),e) -> injectRight (o,injectLeft ((i,o),e))) ^>> left f >>^ eject
  right (CacheArrow f) = CacheArrow $ (\((i,o),e) -> injectRight ((i,o),injectLeft (o,e))) ^>> right f >>^ eject

instance ArrowApply (CacheArrow i o) where
  app = CacheArrow $ (\(io,(CacheArrow f,x)) -> (f,(io,x))) ^>> app

instance (Eq x, Hashable x, LowerBounded y, Complete y) => ArrowFix x y (CacheArrow x y) where
  fixA f = proc x -> do
    old <- getOutCache -< ()
    setOutCache -< bottom
    y <- localInCache (fix (f . memoize)) -< (old,x)
    new <- getOutCache -< ()
    if new ⊑ old -- We are in the reductive set of `f` and have overshot the fixpoint
    then returnA -< y
    else fixA f -< x

memoize :: (Eq x, Hashable x, LowerBounded y, Complete y) => CacheArrow x y x y -> CacheArrow x y x y
memoize f = proc x -> do
  m <- lookupOutCache -< x
  case m of
    Just y -> returnA -< y
    Nothing -> do
      yOld <- lookupInCache -< x
      writeOutCache -< (x, fromMaybe bottom yOld)
      y <- f -< x
      updateOutCache -< (x, y)
      returnA -< y

lookupOutCache :: (Eq x, Hashable x) => CacheArrow x y x (Maybe y)
lookupOutCache = CacheArrow $ \((_,o),x) -> (o,S.lookup x o)

lookupInCache :: (Eq x, Hashable x) => CacheArrow x y x (Maybe y)
lookupInCache = CacheArrow $ \((i,o),x) -> (o,S.lookup x i)

writeOutCache :: (Eq x, Hashable x) => CacheArrow x y (x,y) ()
writeOutCache = CacheArrow $ \((_,o),(x,y)) -> (S.insert x y o,())

getOutCache :: CacheArrow x y () (Store x y)
getOutCache = CacheArrow $ (\((_,o),()) -> (o,o))

setOutCache :: CacheArrow x y (Store x y) ()
setOutCache = CacheArrow $ (\((_,_),o) -> (o,()))

localInCache :: CacheArrow x y x y -> CacheArrow x y (Store x y,x) y
localInCache (CacheArrow f) = CacheArrow (\((_,o),(i,x)) -> f ((i,o),x))

updateOutCache :: (Eq x, Hashable x, Complete y) => CacheArrow x y (x,y) ()
updateOutCache = CacheArrow $ \((_,o),(x,y)) -> (S.insertWith (⊔) x y o,())

deriving instance PreOrd (((Store a b,Store a b),x) -> (Store a b,y)) => PreOrd (CacheArrow a b x y)
deriving instance Complete (((Store a b,Store a b),x) -> (Store a b,y)) => Complete (CacheArrow a b x y)
deriving instance CoComplete (((Store a b,Store a b),x) -> (Store a b,y)) => CoComplete (CacheArrow a b x y)
deriving instance LowerBounded (((Store a b,Store a b),x) -> (Store a b,y)) => LowerBounded (CacheArrow a b x y)
deriving instance UpperBounded (((Store a b,Store a b),x) -> (Store a b,y)) => UpperBounded (CacheArrow a b x y)
