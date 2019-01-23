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

-- | Computation that produces a set of results.
newtype PowT c x y = PowT { runPowT :: c x (A.Pow y)}

instance ArrowTrans PowT where
  type Dom1 PowT x y = x
  type Cod1 PowT x y = A.Pow y
  lift = PowT
  unlift = runPowT

instance ArrowLift PowT where
  lift' f = PowT $ f >>^ A.singleton

mapPow :: ArrowChoice c => c x y -> c (A.Pow x) (A.Pow y)
mapPow f = proc (A.Pow s) -> case viewl s of
  EmptyL -> returnA -< A.empty
  (x :< xs) -> do
    p <- f -< x
    A.Pow ps <- mapPow f -< A.Pow xs
    returnA -< A.Pow (p <| ps)

instance ArrowChoice c => Category (PowT c) where
  id = lift' id
  PowT f . PowT g = PowT $ g >>> mapPow f >>^ join

instance ArrowChoice c => Arrow (PowT c) where
  arr f = lift' (arr f)
  first (PowT f) = PowT $ first f >>^ \(pow,n) -> A.cartesian (pow, A.singleton n)
  second (PowT f) = PowT $ second f >>^ \(n,pow) -> A.cartesian (A.singleton n, pow)

instance ArrowChoice c => ArrowChoice (PowT c) where
  left (PowT f) = PowT $ left f >>^ strength1
  right (PowT f) = PowT $ right f >>^ strength2

instance (ArrowChoice c, ArrowApply c) => ArrowApply (PowT c) where
  app = PowT $ first runPowT ^>> app

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

instance (ArrowChoice c, ArrowFix x (A.Pow y) c) => ArrowFix x y (PowT c) where
  fix f = PowT (fix (runPowT . f . PowT))

deriving instance PreOrd (c x (A.Pow y)) => PreOrd (PowT c x y)
deriving instance LowerBounded (c x (A.Pow y)) => LowerBounded (PowT c x y)
deriving instance Complete (c x (A.Pow y)) => Complete (PowT c x y)
deriving instance CoComplete (c x (A.Pow y)) => CoComplete (PowT c x y)
deriving instance UpperBounded (c x (A.Pow y)) => UpperBounded (PowT c x y)
