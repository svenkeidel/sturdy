{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Arrows #-}
module Control.Arrow.Transformer.Abstract.Powerset(PowT(..)) where

import           Prelude hiding (id,(.),lookup,fail)

import           Control.Arrow
import           Control.Arrow.Abstract.Join
import           Control.Arrow.Deduplicate
import           Control.Arrow.Environment as Env
import           Control.Arrow.Fail
import           Control.Arrow.Trans
import           Control.Arrow.Reader
import           Control.Arrow.State
import           Control.Arrow.Fix
import           Control.Category
import           Control.Monad (join)

import qualified Data.Abstract.Powerset as A
import           Data.Monoidal
import           Data.Order
import           Data.Identifiable
import           Data.Sequence hiding (lookup)
import           Data.Profunctor

-- | Computation that produces a set of results.
newtype PowT c x y = PowT { runPowT :: c x (A.Pow y)}

instance (Profunctor c, Arrow c) => Profunctor (PowT c) where
  dimap f g h = lift $ dimap f (fmap g) (unlift h)
  {-# INLINE dimap #-}
  lmap f h = lift $ lmap f (unlift h)
  {-# INLINE lmap #-}
  rmap g h = lift $ rmap (fmap g) (unlift h)
  {-# INLINE rmap #-}

instance ArrowTrans PowT where
  type Dom PowT x y = x
  type Cod PowT x y = A.Pow y
  lift = PowT
  {-# INLINE lift #-}
  unlift = runPowT
  {-# INLINE unlift #-}

instance ArrowLift PowT where
  lift' f = lift $ rmap A.singleton f
  {-# INLINE lift' #-}

mapPow :: ArrowChoice c => c x y -> c (A.Pow x) (A.Pow y)
mapPow f = proc (A.Pow s) -> case viewl s of
  EmptyL -> returnA -< A.empty
  (x :< xs) -> do
    p <- f -< x
    A.Pow ps <- mapPow f -< A.Pow xs
    returnA -< A.Pow (p <| ps)

instance (ArrowChoice c, Profunctor c) => Category (PowT c) where
  id = lift' id
  {-# INLINE id #-}
  f . g = lift $ rmap join (unlift g >>> mapPow (unlift f))
  {-# INLINE (.) #-}

instance (ArrowChoice c, Profunctor c) => Arrow (PowT c) where
  arr f = lift' (arr f)
  {-# INLINE arr #-}
  first f = lift $ rmap (\(pow,n) -> A.cartesian (pow, A.singleton n)) (first (unlift f))
  {-# INLINE first #-}
  second f = lift $ rmap (\(n,pow) -> A.cartesian (A.singleton n, pow)) (second (unlift f))
  {-# INLINE second #-}
  f &&& g = lift $ rmap A.cartesian (unlift f &&& unlift g)
  {-# INLINE (&&&) #-}
  f *** g = lift $ rmap A.cartesian (unlift f *** unlift g)
  {-# INLINE (***) #-}

instance (ArrowChoice c, Profunctor c) => ArrowChoice (PowT c) where
  left f = lift $ rmap strength1 $ left (unlift f)
  {-# INLINE left #-}
  right f = lift $ rmap strength2 $ right (unlift f)
  {-# INLINE right #-}
  f ||| g = lift $ unlift f ||| unlift g
  f +++ g = lift $ rmap merge $ unlift f +++ unlift g
    where
      merge :: Either (A.Pow a) (A.Pow b) -> A.Pow (Either a b)
      merge (Left e) = fmap Left e
      merge (Right e) = fmap Right e

instance (ArrowChoice c, Profunctor c, ArrowApply c) => ArrowApply (PowT c) where
  app = PowT $ lmap (first unlift) app

instance (ArrowChoice c, ArrowReader r c) => ArrowReader r (PowT c) where
  ask = lift' ask
  local (PowT f) = PowT $ local f

instance (ArrowChoice c, ArrowState s c) => ArrowState s (PowT c) where
  get = lift' get
  put = lift' put

instance (ArrowChoice c, ArrowFail e c) => ArrowFail e (PowT c) where
  fail = lift' fail

instance (ArrowChoice c, ArrowEnv x y env c) => ArrowEnv x y env (PowT c) where
  type Join (PowT c) x y = Env.Join c x (A.Pow y)
  lookup (PowT f) (PowT g) = PowT (lookup f g)
  getEnv = lift' getEnv
  extendEnv = lift' extendEnv
  localEnv (PowT f) = PowT $ localEnv f

instance (ArrowChoice c, ArrowDeduplicate x y c, Identifiable y) => ArrowDeduplicate x y (PowT c) where
  dedup (PowT f) = PowT $ A.dedup ^<< f

instance (ArrowChoice c, ArrowJoin c) => ArrowJoin (PowT c) where
  joinWith _ (PowT f) (PowT g) = PowT $ joinWith A.union f g

type instance Fix x y (PowT c) = PowT (Fix (Dom PowT x y) (Cod PowT x y) c)
instance (ArrowChoice c, ArrowFix x (A.Pow y) c) => ArrowFix x y (PowT c) where
  fix f = PowT (fix (runPowT . f . PowT))

deriving instance PreOrd (c x (A.Pow y)) => PreOrd (PowT c x y)
deriving instance LowerBounded (c x (A.Pow y)) => LowerBounded (PowT c x y)
deriving instance Complete (c x (A.Pow y)) => Complete (PowT c x y)
deriving instance CoComplete (c x (A.Pow y)) => CoComplete (PowT c x y)
deriving instance UpperBounded (c x (A.Pow y)) => UpperBounded (PowT c x y)
