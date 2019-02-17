{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Arrows #-}
module Control.Arrow.Transformer.Abstract.Powerset(PowT,runPowT) where

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
import           Control.Arrow.Const
import           Control.Arrow.Store
import           Control.Arrow.Except
import           Control.Arrow.Transformer.Kleisli
import           Control.Category

import qualified Data.Abstract.Powerset as A
import           Data.Order
import           Data.Identifiable
import           Data.Profunctor

-- | Computation that produces a set of results.
newtype PowT c x y = PowT { unPowT :: KleisliT A.Pow c x y}
  deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowTrans, ArrowLift, ArrowState s, ArrowReader r,
            ArrowConst r, ArrowEnv a b e', ArrowStore a b, ArrowFail e', ArrowExcept e')

runPowT :: PowT c x y -> c x (A.Pow y)
runPowT = runKleisliT . unPowT

instance (ArrowChoice c, ArrowDeduplicate x y c, Identifiable y) => ArrowDeduplicate x y (PowT c) where
  dedup f = lift $ rmap A.dedup (unlift f)

instance (ArrowChoice c, Profunctor c, ArrowApply c) => ArrowApply (PowT c) where app = lift $ lmap (first unlift) app
type instance Fix x y (PowT c) = PowT (Fix (Dom PowT x y) (Cod PowT x y) c)
deriving instance (ArrowChoice c, ArrowFix x (A.Pow y) c) => ArrowFix x y (PowT c)

deriving instance PreOrd (c x (A.Pow y)) => PreOrd (PowT c x y)
deriving instance LowerBounded (c x (A.Pow y)) => LowerBounded (PowT c x y)
deriving instance Complete (c x (A.Pow y)) => Complete (PowT c x y)
deriving instance CoComplete (c x (A.Pow y)) => CoComplete (PowT c x y)
deriving instance UpperBounded (c x (A.Pow y)) => UpperBounded (PowT c x y)

instance (ArrowChoice c, ArrowJoin c) => ArrowJoin (PowT c) where
  joinWith _ f g = lift $ joinWith A.union (unlift f) (unlift g)
