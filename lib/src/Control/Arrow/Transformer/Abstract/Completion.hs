{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Transformer.Abstract.Completion(CompletionT,runCompletionT) where

import Prelude hiding ((.),id,lookup,fail)

import Control.Arrow
import Control.Arrow.Environment
import Control.Arrow.Closure
import Control.Arrow.Except
import Control.Arrow.Fail
import Control.Arrow.Fix
import Control.Arrow.Reader
import Control.Arrow.State
import Control.Arrow.Store
import Control.Arrow.Trans
import Control.Arrow.Const
import Control.Arrow.Order
import Control.Arrow.Transformer.Kleisli
import Control.Category

import Data.Abstract.FreeCompletion
import Data.Abstract.Widening

import Data.Profunctor
import Data.Profunctor.Unsafe((.#))
import Data.Coerce

-- | Allows to describe computations over non-completely ordered types.
-- E.g. allows to join a computation of type 'c x [y]'.
newtype CompletionT c x y = CompletionT (KleisliT FreeCompletion c x y)
  deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowTrans, ArrowLift, ArrowRun,
            ArrowConst r, ArrowState s, ArrowReader r,
            ArrowEnv var val, ArrowClosure expr cls, ArrowStore a b,
            ArrowFail e, ArrowExcept e)

runCompletionT :: CompletionT c x y -> c x (FreeCompletion y)
runCompletionT = coerce
{-# INLINE runCompletionT #-}

instance (ArrowChoice c, ArrowApply c, Profunctor c) => ArrowApply (CompletionT c) where
  app = lift (app .# first coerce)
type instance Fix (CompletionT c) x y = CompletionT (Fix c x (FreeCompletion y))
deriving instance (ArrowFix (Underlying (CompletionT c) x y)) => ArrowFix (CompletionT c x y)

deriving instance (ArrowChoice c, ArrowComplete (FreeCompletion y) c) => ArrowComplete y (CompletionT c)
deriving instance (ArrowChoice c, ArrowLowerBounded c) => ArrowLowerBounded (CompletionT c)

instance (ArrowChoice c, ArrowJoin c) => ArrowJoin (CompletionT c) where
  joinSecond lub f g = lift $ joinSecond (toJoin widening lub) (return . f) (unlift g)
  {-# INLINE joinSecond #-}
