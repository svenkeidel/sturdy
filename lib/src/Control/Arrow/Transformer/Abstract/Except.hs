{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE GADTs #-}
module Control.Arrow.Transformer.Abstract.Except(ExceptT,runExceptT) where

import Prelude hiding (id,lookup,(.),read,fail)

import Control.Category
import Control.Arrow hiding (ArrowMonad)
import Control.Arrow.Const
import Control.Arrow.Environment as Env
import Control.Arrow.Except
import Control.Arrow.Fail
import Control.Arrow.Fix
import Control.Arrow.Order
import Control.Arrow.Reader
import Control.Arrow.State
import Control.Arrow.Store as Store
import Control.Arrow.Trans
import Control.Arrow.Transformer.Kleisli

import Data.Abstract.Except

import Data.Order(Complete(..))
import Data.Profunctor
import Data.Profunctor.Unsafe((.#))
import Data.Coerce

newtype ExceptT e c x y = ExceptT (KleisliT (Except e) c x y)
  deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowTrans, ArrowLift, ArrowRun, ArrowJoin, ArrowLowerBounded,
            ArrowConst r, ArrowState s, ArrowReader r,
            ArrowEnv var val, ArrowClosure var val env, ArrowStore a b,
            ArrowFail e')

runExceptT :: ExceptT e c x y -> c x (Except e y)
runExceptT = coerce
{-# INLINE runExceptT #-}

instance (Complete e, ArrowChoice c, ArrowJoin c) => ArrowExcept e (ExceptT e c) where
  type Join y (ExceptT e c) = ArrowComplete (Except e y) c
  throw = lift $ arr Fail
  try f g h = lift $ proc x -> do
    e <- unlift f -< x
    case e of
      Success y          -> unlift g -< y
      Fail er            -> unlift h -< (x,er)
      SuccessOrFail er y -> (unlift g -< y) <⊔> (unlift h -< (x,er))
  {-# INLINE throw #-}
  {-# INLINE try #-}

instance (Complete e, ArrowJoin c, ArrowChoice c, ArrowApply c, Profunctor c) => ArrowApply (ExceptT e c) where
  app = lift (app .# first coerce)
  {-# INLINE app #-}

type instance Fix (ExceptT e c) x y = ExceptT e (Fix c x (Except e y))
instance ArrowFix (Underlying (ExceptT e c) x y) => ArrowFix (ExceptT e c x y)
deriving instance (Complete e, ArrowChoice c, ArrowJoin c, ArrowComplete (Except e y) c) => ArrowComplete y (ExceptT e c)
