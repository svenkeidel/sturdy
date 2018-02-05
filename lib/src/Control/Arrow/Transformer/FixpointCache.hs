{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Arrow.Transformer.FixpointCache(CacheArrow,runCacheArrow,liftCache) where

import           Prelude hiding (id,(.),lookup)
import           Data.Function (fix)

import           Control.Arrow
import           Control.Arrow.Class.Fail (ArrowFail(..))
import           Control.Arrow.Class.Reader
import           Control.Arrow.Class.State
import           Control.Arrow.Class.Fix
import           Control.Arrow.Class.Environment
import           Control.Arrow.Utils
import           Control.Category

import           Data.Hashable (Hashable)
import           Data.Maybe
import           Data.Order
import           Data.Store (Store)
import qualified Data.Store as S

newtype CacheArrow a b c x y = CacheArrow (c ((Store a b,Store a b),x) (Store a b,y))

runCacheArrow :: Arrow c => CacheArrow a b c x y -> c x y
runCacheArrow (CacheArrow f) = (\x -> ((S.empty,S.empty),x)) ^>> f >>^ snd

liftCache :: Arrow c => c x y -> CacheArrow a b c x y
liftCache f = CacheArrow ((\((_,o),x) -> (o,x)) ^>> second f)

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

instance ArrowApply c => ArrowApply (CacheArrow i o c) where
  app = CacheArrow $ (\(io,(CacheArrow f,x)) -> (f,(io,x))) ^>> app

instance ArrowState s c => ArrowState s (CacheArrow i o c) where
  getA = liftCache getA
  putA = liftCache putA

instance ArrowReader r c => ArrowReader r (CacheArrow i o c) where
  askA = liftCache askA
  localA (CacheArrow f) = CacheArrow $ (\((i,o),(r,x)) -> (r, ((i,o),x))) ^>> localA f

instance ArrowFail e c => ArrowFail e (CacheArrow i o c) where
  failA = liftCache failA

instance ArrowEnv a b env c => ArrowEnv a b env (CacheArrow x y c) where
  lookup = liftCache lookup
  getEnv = liftCache getEnv
  extendEnv = liftCache extendEnv
  localEnv (CacheArrow f) = CacheArrow $ (\(s,(env,a)) -> (env,(s,a))) ^>> localEnv f

instance (Eq a, Hashable a, LowerBounded b, Complete b, ArrowChoice c) => ArrowFix a b (CacheArrow a b c) where
  fixA f = proc x -> do
    y <- retireCache (fix (f . memoize)) -< x
    fp <- reachedFixpoint -< ()
    if fp
    then returnA -< y
    else fix f -< x

memoize :: (Eq a, Hashable a, LowerBounded b, Complete b, ArrowChoice c) => CacheArrow a b c a b -> CacheArrow a b c a b
memoize f = proc x -> do
  m <- askCache -< x
  case m of
    Just y -> returnA -< y
    Nothing -> do
      initializeCache -< x
      y <- f -< x
      updateCache -< (x,y)
      returnA -< y

askCache :: (Eq a, Hashable a, Arrow c) => CacheArrow a b c a (Maybe b)
askCache = CacheArrow $ arr $ \((_,o),x) -> (o,S.lookup x o)

retireCache :: (Eq a, Hashable a, LowerBounded b, Arrow c) => CacheArrow a b c x y -> CacheArrow a b c x y
retireCache (CacheArrow f) = CacheArrow $ (\((_,o),x) -> ((o,bottom),x)) ^>> f

initializeCache :: (Eq a, Hashable a, LowerBounded b, Arrow c) => CacheArrow a b c a ()
initializeCache = CacheArrow $ arr $ \((i,o),x) -> (S.insert x (fromMaybe bottom (S.lookup x i)) o,())

updateCache :: (Eq a, Hashable a, Complete b, Arrow c) => CacheArrow a b c (a,b) ()
updateCache = CacheArrow $ arr $ \((_,o),(x,y)) -> (S.insertWith (⊔) x y o,())

reachedFixpoint :: (Eq a, Hashable a, LowerBounded b, Arrow c) => CacheArrow a b c () Bool
reachedFixpoint = CacheArrow $ arr $ \((i,o),()) -> (o,o ⊑ i)

deriving instance PreOrd (c ((Store a b,Store a b),x) (Store a b,y)) => PreOrd (CacheArrow a b c x y)
deriving instance Complete (c ((Store a b,Store a b),x) (Store a b,y)) => Complete (CacheArrow a b c x y)
deriving instance CoComplete (c ((Store a b,Store a b),x) (Store a b,y)) => CoComplete (CacheArrow a b c x y)
deriving instance LowerBounded (c ((Store a b,Store a b),x) (Store a b,y)) => LowerBounded (CacheArrow a b c x y)
deriving instance UpperBounded (c ((Store a b,Store a b),x) (Store a b,y)) => UpperBounded (CacheArrow a b c x y)
