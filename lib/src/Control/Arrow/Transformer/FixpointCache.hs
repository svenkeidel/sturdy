{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE CPP #-}
-- {-# OPTIONS_GHC -DTRACE #-}
module Control.Arrow.Transformer.FixpointCache(CacheArrow,runCacheArrow,runCacheArrow',liftCache) where

import           Prelude hiding (id,(.),lookup)

import           Control.Arrow hiding (loop)
import           Control.Arrow.Class.Fix
import           Control.Arrow.Utils
import           Control.Category

import           Data.Hashable (Hashable)
import           Data.Maybe
import           Data.Order
import           Data.Store (Store)
import qualified Data.Store as S

#ifdef TRACE
import           Debug.Trace
import           Text.Printf
#endif

newtype CacheArrow a b x y = CacheArrow ((Store a b,x) -> (Store a b,y))

runCacheArrow :: CacheArrow a b x y -> (x -> y)
runCacheArrow f = runCacheArrow' f >>^ snd

runCacheArrow' :: CacheArrow a b x y -> (x -> (Store a b,y))
runCacheArrow' (CacheArrow f) = (\x -> ((S.empty),x)) ^>> f

liftCache :: (x -> y) -> CacheArrow a b x y
liftCache f = CacheArrow ((\(o,x) -> (o,x)) ^>> second f)

instance Category (CacheArrow i o) where
  id = liftCache id
  CacheArrow f . CacheArrow g = CacheArrow $ proc (o,x) -> do
    (o',y) <- g -< (o,x)
    f -< (o',y)

instance Arrow (CacheArrow i o) where
  arr f = liftCache (arr f)
  first (CacheArrow f) = CacheArrow $ (\(o,(x,y)) -> ((o,x),y)) ^>> first f >>^ (\((o,x'),y) -> (o,(x',y)))
  second (CacheArrow f) = CacheArrow $ (\(o,(x,y)) -> (x,(o,y))) ^>> second f >>^ (\(x,(o,y')) -> (o,(x,y')))

instance ArrowChoice (CacheArrow i o) where
  left (CacheArrow f) = CacheArrow $ (\(o,e) -> injectRight (o,injectLeft (o,e))) ^>> left f >>^ eject
  right (CacheArrow f) = CacheArrow $ (\(o,e) -> injectRight (o,injectLeft (o,e))) ^>> right f >>^ eject

instance ArrowApply (CacheArrow i o) where
  app = CacheArrow $ (\(o,(CacheArrow f,x)) -> (f,(o,x))) ^>> app

#ifdef TRACE
instance (Show x, Show y, Eq x, Hashable x, LowerBounded y, Complete y)
  => ArrowFix x y (CacheArrow x y) where
  fixA f = proc x -> do
    m <- lookupCache -< x
    case m of
      Just y -> do
        returnA -< trace (printf "%s <- memoize -< %s\t(HIT)" (show y) (show x)) y
      Nothing -> do
        updateCache -< trace (printf "memoize -< %s\t\t(MISS)\nout(%s) := ⊥" (show x) (show x)) (x, bottom)
        y <- loop -< x
        returnA -< trace (printf "%s <- memoize -< %s" (show y) (show x)) y
      where
        loop = proc x -> do
          yOld <- fromJust ^<< lookupCache -< x
          y <- f (fixA f) -< trace (printf "%s -< %s" name (show x)) x
          yNew <- updateCache -< trace (printf "%s <- %s -< %s" (show y) name (show x)) (x, y)
          if trace (printf "out(%s) := %s ⊔ %s = %s" (show x) (show yOld) (show y) (show yNew)) $
             yNew ⊑ yOld
          then returnA -< trace (printf "%s ⊑ %s\t\t\t(RETURN)" (show yNew) (show yOld)) yNew
          else loop -< trace (printf "%s ⋢ %s\t\t\t(REPEAT)" (show yNew) (show yOld)) x
        name = "fact"
#else
instance (Eq x, Hashable x, LowerBounded y, Complete y)
  => ArrowFix x y (CacheArrow x y) where
  fixA f = proc x -> do
    m <- lookupCache -< x
    case m of
      Just y -> returnA -< y
      Nothing -> do
        updateCache -< (x, bottom)
        loop -< x
      where
        loop = proc x -> do
          yOld <- fromJust ^<< lookupCache -< x
          y <- f (fixA f) -< x
          yNew <- updateCache -< (x, y)
          if yNew ⊑ yOld
          then returnA -< yNew
          else loop -< x
#endif

lookupCache :: (Eq x, Hashable x) => CacheArrow x y x (Maybe y)
lookupCache = CacheArrow $ \(o,x) -> (o,S.lookup x o)

updateCache :: (Eq x, Hashable x, Complete y) => CacheArrow x y (x,y) y
updateCache = CacheArrow $ \(o,(x,y)) -> case S.lookup x o of
  Just y' -> let y'' = y ⊔ y' in (S.insert x y'' o,y'')
  Nothing -> (S.insert x y o,y)

deriving instance PreOrd ((Store a b,x) -> (Store a b,y)) => PreOrd (CacheArrow a b x y)
deriving instance Complete ((Store a b,x) -> (Store a b,y)) => Complete (CacheArrow a b x y)
deriving instance CoComplete ((Store a b,x) -> (Store a b,y)) => CoComplete (CacheArrow a b x y)
deriving instance LowerBounded ((Store a b,x) -> (Store a b,y)) => LowerBounded (CacheArrow a b x y)
deriving instance UpperBounded ((Store a b,x) -> (Store a b,y)) => UpperBounded (CacheArrow a b x y)
