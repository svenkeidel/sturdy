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
import Data.Abstract.Widening (toJoin)

import Data.Profunctor
import Data.Profunctor.Unsafe((.#))
import Data.Coerce

-- | Arrow that propagates non-terminating computations.
newtype TerminatingT c x y = TerminatingT (KleisliT Terminating c x y) 
  deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowTrans, ArrowLift, ArrowRun,
            ArrowConst r, ArrowState s, ArrowReader r,
            ArrowEnv var val, ArrowClosure var val env, ArrowStore addr val)

runTerminatingT :: TerminatingT c x y -> c x (Terminating y)
runTerminatingT = coerce
{-# INLINE runTerminatingT #-}

instance (ArrowChoice c, Profunctor c, ArrowApply c) => ArrowApply (TerminatingT c) where
  app = lift (app .# first coerce)

type instance Fix x y (TerminatingT c) = TerminatingT (Fix (Dom TerminatingT x y) (Cod TerminatingT x y) c)
deriving instance (ArrowChoice c, ArrowFix (Dom TerminatingT x y) (Cod TerminatingT x y) c) => ArrowFix x y (TerminatingT c)

instance (ArrowChoice c, Profunctor c) => ArrowLowerBounded (TerminatingT c) where
  bottom = lift $ arr (\_ -> NonTerminating)

instance (ArrowChoice c, ArrowJoin c) => ArrowJoin (TerminatingT c) where
  join lub' f g = lift $ join (toJoin widening lub') (unlift f) (unlift g)

deriving instance (ArrowChoice c, ArrowComplete (Terminating y) c) => ArrowComplete y (TerminatingT c)
