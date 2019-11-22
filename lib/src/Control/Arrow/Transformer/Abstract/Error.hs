{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Transformer.Abstract.Error(ErrorT,runErrorT) where

import Prelude hiding (id,lookup,(.),read,fail)

import Control.Arrow
import Control.Arrow.Cont
import Control.Arrow.Const
import Control.Arrow.Environment as Env
import Control.Arrow.Closure as Cls
import Control.Arrow.Fail
import Control.Arrow.Trans
import Control.Arrow.Reader
import Control.Arrow.State
import Control.Arrow.Store as Store
import Control.Arrow.Except as Exc
import Control.Arrow.Fix
import Control.Arrow.Order
import Control.Arrow.Transformer.Kleisli
import Control.Category

import qualified Data.Order as O
import Data.Abstract.Error
import Data.Abstract.Widening

import Data.Profunctor
import Data.Profunctor.Unsafe((.#))
import Data.Coerce

newtype ErrorT e c x y = ErrorT (KleisliT (Error e) c x y)
  deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowTrans, ArrowLift, ArrowRun,
            ArrowCont, ArrowConst r, ArrowState s, ArrowReader r,
            ArrowEnv var val, ArrowLetRec var val, ArrowClosure expr cls, ArrowStore a b,
            ArrowExcept e')

runErrorT :: ErrorT e c x y -> c x (Error e y)
runErrorT = coerce
{-# INLINE runErrorT #-}

instance (ArrowChoice c, Profunctor c) => ArrowFail e (ErrorT e c) where
  fail = lift $ arr Fail
  {-# INLINE fail #-}

instance (ArrowChoice c, ArrowApply c, Profunctor c) => ArrowApply (ErrorT e c) where
  app = lift (app .# first coerce)
  {-# INLINE app #-}

type instance Fix (ErrorT e c) x y = ErrorT e (Fix c x (Error e y))
deriving instance (ArrowFix (Underlying (ErrorT e c) x y)) => ArrowFix (ErrorT e c x y)

instance (ArrowChoice c, ArrowLowerBounded c) => ArrowLowerBounded (ErrorT e c) where
  bottom = lift bottom
  {-# INLINE bottom #-}

deriving instance (ArrowChoice c, ArrowComplete (Error e y) c) => ArrowComplete y (ErrorT e c)

instance (O.Complete e, ArrowChoice c, ArrowJoin c) => ArrowJoin (ErrorT e c) where
  joinSecond lub f g = lift $ joinSecond (toJoin2 widening (O.⊔) lub) (Success . f) (unlift g)
  {-# INLINE joinSecond #-}
