{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Arrows #-}
module Control.Arrow.Transformer.Abstract.Floor where

import Prelude hiding (id,(.),lookup)
import Control.Category
import Control.Arrow
import Control.Arrow.Environment
import Control.Arrow.Fail
import Control.Arrow.Lift
import Control.Arrow.Fix
import Control.Arrow.Reader
import Control.Arrow.State

import Data.Abstract.Floored
import Data.Order
import Data.Utils

newtype Floor c x y = Floor { runFloor :: c x (Floored y) }

instance ArrowLift Floor where
  lift f = Floor (f >>> arr Greater)

instance ArrowChoice c => Category (Floor c) where
  id = lift id
  Floor f . Floor g = Floor $ proc x -> do
    ey <- g -< x
    case ey of
      Bottom -> returnA -< Bottom
      Greater y -> f -< y

instance ArrowChoice c => Arrow (Floor c) where
  arr f = lift (arr f)
  first (Floor f) = Floor $ first f >>^ strength1
  second (Floor f) = Floor $ second f >>^ strength2

instance ArrowChoice c => ArrowChoice (Floor c) where
  left (Floor f) = Floor $ left f >>^ costrength1
  right (Floor f) = Floor $ right f >>^ costrength2

instance (ArrowChoice c, ArrowApply c) => ArrowApply (Floor c) where
  app = Floor $ first runFloor ^>> app

instance (ArrowChoice c, ArrowState s c) => ArrowState s (Floor c) where
  getA = lift getA
  putA = lift putA

instance (ArrowChoice c, ArrowFail e c) => ArrowFail e (Floor c) where
  failA = lift failA

instance (ArrowChoice c, ArrowReader r c) => ArrowReader r (Floor c) where
  askA = lift askA
  localA (Floor f) = Floor (localA f)

instance (ArrowChoice c, ArrowEnv x y env c) => ArrowEnv x y env (Floor c) where
  lookup = lift lookup
  getEnv = lift getEnv
  extendEnv = lift extendEnv
  localEnv (Floor f) = Floor (localEnv f)

instance (ArrowChoice c, ArrowFix x (Floored y) c) => ArrowFix x y (Floor c) where
  fixA f = Floor (fixA (runFloor . f . Floor))

deriving instance PreOrd (c x (Floored y)) => PreOrd (Floor c x y)
deriving instance LowerBounded (c x (Floored y)) => LowerBounded (Floor c x y)
deriving instance Complete (c x (Floored y)) => Complete (Floor c x y)
deriving instance CoComplete (c x (Floored y)) => CoComplete (Floor c x y)
deriving instance UpperBounded (c x (Floored y)) => UpperBounded (Floor c x y)
