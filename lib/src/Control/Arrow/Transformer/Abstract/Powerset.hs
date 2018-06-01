{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Arrows #-}
module Control.Arrow.Transformer.Abstract.Powerset(Powerset(..)) where

import           Prelude hiding (id,(.),lookup)

import           Control.Arrow
import           Control.Arrow.Abstract.Join
import           Control.Arrow.Deduplicate
import           Control.Arrow.Environment
import           Control.Arrow.Fail
import           Control.Arrow.Lift
import           Control.Arrow.Reader
import           Control.Arrow.State
import           Control.Arrow.Fix
import           Control.Category
import           Control.Monad (join)

import qualified Data.Abstract.Powerset as A
import           Data.Monoidal
import           Data.Order
import           Data.Sequence hiding (lookup)

-- | Computation that produces a set of results.
newtype Powerset c x y = Powerset { runPowerset :: c x (A.Pow y)}

instance ArrowLift Powerset where
  lift f = Powerset $ f >>^ A.singleton

mapPow :: ArrowChoice c => c x y -> c (A.Pow x) (A.Pow y)
mapPow f = proc (A.Pow s) -> case viewl s of
  EmptyL -> returnA -< A.empty
  (x :< xs) -> do
    p <- f -< x
    A.Pow ps <- mapPow f -< A.Pow xs
    returnA -< A.Pow (p <| ps)

instance ArrowChoice c => Category (Powerset c) where
  id = lift id
  Powerset f . Powerset g = Powerset $ g >>> mapPow f >>^ join

instance ArrowChoice c => Arrow (Powerset c) where
  arr f = lift (arr f)
  first (Powerset f) = Powerset $ first f >>^ \(pow,n) -> A.cartesian (pow, A.singleton n)
  second (Powerset f) = Powerset $ second f >>^ \(n,pow) -> A.cartesian (A.singleton n, pow)

instance ArrowChoice c => ArrowChoice (Powerset c) where
  left (Powerset f) = Powerset $ left f >>^ strength1
  right (Powerset f) = Powerset $ right f >>^ strength2

instance (ArrowChoice c, ArrowApply c) => ArrowApply (Powerset c) where
  app = Powerset $ first runPowerset ^>> app

instance (ArrowChoice c, ArrowReader r c) => ArrowReader r (Powerset c) where
  askA = lift askA
  localA (Powerset f) = Powerset $ localA f

instance (ArrowChoice c, ArrowState s c) => ArrowState s (Powerset c) where
  getA = lift getA
  putA = lift putA

instance (ArrowChoice c, ArrowFail e c) => ArrowFail e (Powerset c) where
  failA = lift failA

instance (ArrowChoice c, ArrowEnv x y env c) => ArrowEnv x y env (Powerset c) where
  lookup (Powerset f) (Powerset g) = Powerset (lookup f g)
  getEnv = lift getEnv
  extendEnv = lift extendEnv
  localEnv (Powerset f) = Powerset (localEnv f)

instance (ArrowChoice c, ArrowDeduplicate c) => ArrowDeduplicate (Powerset c) where
  dedupA (Powerset f) = Powerset $ proc x -> do
    x' <- f -< x
    returnA -< A.dedup x'

instance (ArrowChoice c, ArrowJoin c) => ArrowJoin (Powerset c) where
  joinWith _ (Powerset f) (Powerset g) = Powerset $ joinWith A.union f g

instance (ArrowChoice c, ArrowFix x (A.Pow y) c) => ArrowFix x y (Powerset c) where
  fixA f = Powerset (fixA (runPowerset . f . Powerset))

deriving instance PreOrd (c x (A.Pow y)) => PreOrd (Powerset c x y)
deriving instance LowerBounded (c x (A.Pow y)) => LowerBounded (Powerset c x y)
deriving instance Complete (c x (A.Pow y)) => Complete (Powerset c x y)
deriving instance CoComplete (c x (A.Pow y)) => CoComplete (Powerset c x y)
deriving instance UpperBounded (c x (A.Pow y)) => UpperBounded (Powerset c x y)
