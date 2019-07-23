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
import Control.Arrow.Environment as Env
import Control.Arrow.Fail
import Control.Arrow.Fix
import Control.Arrow.Trans
import Control.Arrow.Reader
import Control.Arrow.State
import Control.Arrow.Store as Store
import Control.Arrow.Except as Exc
import Control.Arrow.Order
import Control.Arrow.Transformer.Kleisli

import Data.Abstract.Failure
import Data.Abstract.Widening (toJoin)

import Data.Profunctor
import Data.Profunctor.Unsafe((.#))
import Data.Coerce

-- | Describes computations that can fail.
newtype FailureT e c x y = FailureT (KleisliT (Failure e) c x y)
  deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowTrans, ArrowLift, ArrowState s, ArrowReader r,
            ArrowConst r, ArrowEnv a b e', ArrowStore a b, ArrowExcept e', ArrowRun)

runFailureT :: FailureT e c x y -> c x (Failure e y)
runFailureT = coerce
{-# INLINE runFailureT #-}

instance (ArrowChoice c, Profunctor c) => ArrowFail e (FailureT e c) where
  fail = lift $ arr Fail

instance (ArrowChoice c, ArrowApply c, Profunctor c) => ArrowApply (FailureT e c) where
  app = lift (app .# first coerce)

type instance Fix x y (FailureT e c) = FailureT e (Fix (Dom (FailureT e) x y) (Cod (FailureT e) x y) c)
deriving instance (ArrowChoice c, ArrowFix (Dom (FailureT e) x y) (Cod (FailureT e) x y) c) =>
  ArrowFix x y (FailureT e c)

instance (ArrowComplete c, ArrowChoice c) => ArrowComplete (FailureT e c) where
  join lub f g = lift $ join (toJoin widening lub) (unlift f) (unlift g)
