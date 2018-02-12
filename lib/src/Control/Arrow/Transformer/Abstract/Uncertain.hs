{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Arrows #-}
module Control.Arrow.Transformer.Abstract.Uncertain(Uncertain(..)) where

import Prelude hiding (id,lookup)

import Control.Arrow
import Control.Arrow.Deduplicate
import Control.Arrow.Environment
import Control.Arrow.Fail
import Control.Arrow.Lift
import Control.Arrow.Reader
import Control.Arrow.State
import Control.Arrow.Try
import Control.Category

import Data.Abstract.UncertainResult
import Data.Monoidal
import Data.Order

newtype Uncertain c x y = Uncertain { runUncertain :: c x (UncertainResult y)}

instance ArrowLift Uncertain where
  lift f = Uncertain (f >>> arr Success)

instance ArrowChoice c => Category (Uncertain c) where
  id = lift id
  Uncertain f . Uncertain g = Uncertain $ proc x -> do
    g' <- g -< x
    case g' of
      Success a -> f -< a
      SuccessOrFail a -> f -< a
      Fail -> returnA -< Fail

instance ArrowChoice c => Arrow (Uncertain c) where
  arr f = lift (arr f)
  first (Uncertain f) = Uncertain $ first f >>^ strength1
  second (Uncertain f) = Uncertain $ second f >>^ strength2

instance ArrowChoice c => ArrowChoice (Uncertain c) where
  left (Uncertain f) = Uncertain $ left f >>^ strength1
  right (Uncertain f) = Uncertain $ right f >>^ strength2

instance (ArrowApply c, ArrowChoice c) => ArrowApply (Uncertain c) where
  app = Uncertain $ first runUncertain ^>> app

instance (ArrowChoice c, ArrowState s c) => ArrowState s (Uncertain c) where
  getA = lift getA
  putA = lift putA

instance ArrowChoice c => ArrowFail () (Uncertain c) where
  failA = Uncertain $ arr (const Fail)

instance (ArrowChoice c, ArrowReader r c) => ArrowReader r (Uncertain c) where
  askA = lift askA
  localA (Uncertain f) = Uncertain (localA f)

instance (ArrowChoice c, ArrowEnv x y env c) => ArrowEnv x y env (Uncertain c) where
  lookup = lift lookup
  getEnv = lift getEnv
  extendEnv = lift extendEnv
  localEnv (Uncertain f) = Uncertain (localEnv f)

instance (ArrowChoice c, Complete (c (y,x) (UncertainResult z))) => ArrowTry x y z (Uncertain c) where
  tryA (Uncertain f) (Uncertain g) (Uncertain h) = Uncertain $ proc x -> do
    e <- f -< x
    case e of
      Success y -> g -< y
      SuccessOrFail y -> joined g h -< (y,x)
      Fail -> h -< x

instance ArrowChoice c => ArrowDeduplicate (Uncertain c) where
  dedupA = returnA

deriving instance PreOrd (c x (UncertainResult y)) => PreOrd (Uncertain c x y)
deriving instance LowerBounded (c x (UncertainResult y)) => LowerBounded (Uncertain c x y)
deriving instance Complete (c x (UncertainResult y)) => Complete (Uncertain c x y)
deriving instance CoComplete (c x (UncertainResult y)) => CoComplete (Uncertain c x y)
deriving instance UpperBounded (c x (UncertainResult y)) => UpperBounded (Uncertain c x y)
