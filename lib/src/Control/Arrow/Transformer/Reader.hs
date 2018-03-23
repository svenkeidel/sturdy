{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
module Control.Arrow.Transformer.Reader(Reader(..)) where

import Prelude hiding (id,(.),lookup)

import Control.Arrow
import Control.Arrow.Environment
import Control.Arrow.Fail
import Control.Arrow.Fix
import Control.Arrow.Reader
import Control.Arrow.State
import Control.Arrow.Deduplicate
import Control.Arrow.Try
import Control.Arrow.Lift
import Control.Arrow.Utils
import Control.Category

import Data.Order

newtype Reader r c x y = Reader { runReader :: c (r,x) y }

instance ArrowLift (Reader r) where
  lift f = Reader (pi2 >>> f)

instance Arrow c => Category (Reader r c) where
  id = lift id
  Reader f . Reader g = Reader $ (\(r,x) -> (r,(r,x))) ^>> f . second g

instance Arrow c => Arrow (Reader r c) where
  arr f = lift (arr f)
  first (Reader f) = Reader $ (\(r,(x,y)) -> ((r,x),y)) ^>> first f
  second (Reader f) = Reader $ (\(r,(x,y)) -> (x,(r,y))) ^>> second f

instance ArrowChoice c => ArrowChoice (Reader r c) where
  left (Reader f) = Reader $ injectLeft ^>> left f
  right (Reader f) = Reader $ injectRight ^>> right f

instance ArrowApply c => ArrowApply (Reader r c) where
  app = Reader $ (\(r,(Reader f,b)) -> (f,(r,b))) ^>> app

instance Arrow c => ArrowReader r (Reader r c) where
  askA = Reader pi1
  localA (Reader f) = Reader $ (\(_,(r,x)) -> (r,x)) ^>> f

instance ArrowState s c => ArrowState s (Reader r c) where
  getA = lift getA
  putA = lift putA

instance ArrowFail e c => ArrowFail e (Reader r c) where
  failA = lift failA

instance ArrowEnv x y env c => ArrowEnv x y env (Reader r c) where
  lookup = lift lookup
  getEnv = lift getEnv
  extendEnv = lift extendEnv
  localEnv (Reader f) = Reader ((\(r,(env,a)) -> (env,(r,a))) ^>> localEnv f)

instance ArrowFix (r,x) y c => ArrowFix x y (Reader r c) where
  fixA f = Reader (fixA (runReader . f . Reader))

instance ArrowTry (r,x) (r,y) (r,z) c => ArrowTry x y z (Reader r c) where
  tryA (Reader f) (Reader g) (Reader h) = Reader $
    tryA (pi1 &&& f) (pi1 &&& g) (pi1 &&& h) >>> pi2

instance ArrowZero c => ArrowZero (Reader r c) where
  zeroArrow = lift zeroArrow

instance ArrowPlus c => ArrowPlus (Reader r c) where
  Reader f <+> Reader g = Reader (f <+> g)

instance ArrowDeduplicate c => ArrowDeduplicate (Reader r c) where
  dedupA (Reader f) = Reader (dedupA f)

deriving instance PreOrd (c (r,x) y) => PreOrd (Reader r c x y)
deriving instance LowerBounded (c (r,x) y) => LowerBounded (Reader r c x y)
deriving instance Complete (c (r,x) y) => Complete (Reader r c x y)
deriving instance CoComplete (c (r,x) y) => CoComplete (Reader r c x y)
deriving instance UpperBounded (c (r,x) y) => UpperBounded (Reader r c x y)
