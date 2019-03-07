{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
module Control.Arrow.Transformer.Abstract.Failure(FailureT,runFailureT) where

import Prelude hiding (id,(.),lookup,read)

import Control.Category
import Control.Arrow
import Control.Arrow.Const
import Control.Arrow.Deduplicate
import Control.Arrow.Environment as Env
import Control.Arrow.Fail
import Control.Arrow.Fix
import Control.Arrow.Trans
import Control.Arrow.Reader
import Control.Arrow.State
import Control.Arrow.Store as Store
import Control.Arrow.Except as Exc
import Control.Arrow.Abstract.Join
import Control.Arrow.Transformer.Kleisli

import Data.Order
import Data.Identifiable
import Data.Profunctor
import Data.Abstract.Failure
import Data.Abstract.Widening (toJoin)

-- | Describes computations that can fail.
newtype FailureT e c x y = FailureT { unFailureT :: KleisliT (Failure e) c x y }
  deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowTrans, ArrowLift, ArrowState s, ArrowReader r,
            ArrowConst r, ArrowEnv a b e', ArrowStore a b, ArrowExcept e')

runFailureT :: FailureT e c x y -> c x (Failure e y)
runFailureT = runKleisliT . unFailureT

instance (ArrowChoice c, Profunctor c) => ArrowFail e (FailureT e c) where
  fail = lift $ arr Fail

instance (ArrowChoice c, ArrowApply c, Profunctor c) => ArrowApply (FailureT e c) where app = lift $ lmap (first unlift) app
type instance Fix x y (FailureT e c) = FailureT e (Fix (Dom (FailureT e) x y) (Cod (FailureT e) x y) c)
deriving instance (ArrowChoice c, ArrowFix (Dom (FailureT e) x y) (Cod (FailureT e) x y) c) => ArrowFix x y (FailureT e c)
deriving instance (Identifiable (Cod (FailureT e) x y), ArrowChoice c, ArrowDeduplicate (Dom (FailureT e) x y) (Cod (FailureT e) x y) c) => ArrowDeduplicate x y (FailureT e c)

deriving instance PreOrd (c x (Cod (FailureT e) x y)) => PreOrd (FailureT e c x y)
deriving instance LowerBounded (c (Dom (FailureT e) x y) (Cod (FailureT e) x y)) => LowerBounded (FailureT e c x y)
deriving instance Complete (c (Dom (FailureT e) x y) (Cod (FailureT e) x y)) => Complete (FailureT e c x y)
deriving instance CoComplete (c (Dom (FailureT e) x y) (Cod (FailureT e) x y)) => CoComplete (FailureT e c x y)
deriving instance UpperBounded (c (Dom (FailureT e) x y) (Cod (FailureT e) x y)) => UpperBounded (FailureT e c x y)

instance (ArrowJoin c, ArrowChoice c) => ArrowJoin (FailureT e c) where
  joinWith lub' f g = lift $ joinWith (toJoin widening lub') (unlift f) (unlift g)
