{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Transformer.Abstract.Terminating(TerminatingT,runTerminatingT) where

import Prelude hiding (id,(.),lookup,fail)

import Control.Arrow
import Control.Arrow.Cont
import Control.Arrow.Const
import Control.Arrow.Environment
import Control.Arrow.Closure
import Control.Arrow.Store
import Control.Arrow.Fail
import Control.Arrow.Fix
import Control.Arrow.Reader
import Control.Arrow.State
import Control.Arrow.Trans
import Control.Arrow.Order
import Control.Arrow.Transformer.Kleisli
import Control.Category
import Control.Arrow.LetRec
import Control.Arrow.Fix.Context

import Data.Abstract.Terminating
import Data.Abstract.Widening

import Data.Profunctor.Unsafe
import Data.Coerce

-- | Arrow that propagates non-terminating computations.
newtype TerminatingT c x y = TerminatingT (KleisliT Terminating c x y)
  deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowLift, ArrowTrans, ArrowRun, ArrowFail e,
            ArrowCont, ArrowConst r, ArrowState s, ArrowReader r,
            ArrowEnv var val, ArrowLetRec var val, ArrowClosure expr cls, ArrowStore addr val, ArrowCallSite ctx)

runTerminatingT :: TerminatingT c x y -> c x (Terminating y)
runTerminatingT = coerce
{-# INLINE runTerminatingT #-}

instance (ArrowChoice c, Profunctor c, ArrowApply c) => ArrowApply (TerminatingT c) where
  app = lift (app .# first coerce)
  {-# INLINE app #-}

instance (ArrowFix (Underlying (TerminatingT c) x y), Profunctor c) => ArrowFix (TerminatingT c x y) where
  type Fix (TerminatingT c x y) = Fix (Underlying (TerminatingT c) x y)

instance (ArrowChoice c, Profunctor c) => ArrowLowerBounded y (TerminatingT c) where
  bottom = lift $ arr (const NonTerminating)
  {-# INLINE bottom #-}

deriving instance (ArrowChoice c, ArrowComplete (Terminating y) c) => ArrowComplete y (TerminatingT c)

instance (ArrowChoice c, ArrowJoin c) => ArrowJoin (TerminatingT c) where
  joinSecond lub f g = lift $ joinSecond (toJoin1 widening lub) (return . f) (unlift g)
  {-# INLINE joinSecond #-}
