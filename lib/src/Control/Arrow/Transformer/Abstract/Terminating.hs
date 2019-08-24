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
import Control.Arrow.Const
import Control.Arrow.Environment
import Control.Arrow.Store
import Control.Arrow.Fix
import Control.Arrow.Reader
import Control.Arrow.State
import Control.Arrow.Trans
import Control.Arrow.Order
import Control.Arrow.Transformer.Kleisli
import Control.Category

import Data.Abstract.Terminating

import Data.Profunctor
import Data.Profunctor.Unsafe((.#))
import Data.Coerce

-- | Arrow that propagates non-terminating computations.
newtype TerminatingT c x y = TerminatingT (KleisliT Terminating c x y) 
  deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowTrans, ArrowLift, ArrowRun, ArrowJoin,
            ArrowConst r, ArrowState s, ArrowReader r,
            ArrowEnv var val, ArrowClosure var val env, ArrowStore addr val)

runTerminatingT :: TerminatingT c x y -> c x (Terminating y)
runTerminatingT = coerce
{-# INLINE runTerminatingT #-}

instance (ArrowChoice c, Profunctor c, ArrowApply c) => ArrowApply (TerminatingT c) where
  app = lift (app .# first coerce)
  {-# INLINE app #-}

type instance Fix (TerminatingT c) x y = TerminatingT (Fix c x (Terminating y))
deriving instance (ArrowFix (Underlying (TerminatingT c) x y)) => ArrowFix (TerminatingT c x y)

instance (ArrowChoice c, Profunctor c) => ArrowLowerBounded (TerminatingT c) where
  bottom = lift $ arr (\_ -> NonTerminating)
  {-# INLINE bottom #-}

deriving instance (ArrowChoice c, ArrowComplete (Terminating y) c) => ArrowComplete y (TerminatingT c)
