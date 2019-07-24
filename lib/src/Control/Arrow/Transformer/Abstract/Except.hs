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

import Control.Arrow
import Control.Arrow.Const
import Control.Arrow.Environment as Env
import Control.Arrow.Fail
import Control.Arrow.Trans
import Control.Arrow.Reader
import Control.Arrow.State
import Control.Arrow.Store as Store
import Control.Arrow.Except
import Control.Arrow.Fix
import Control.Arrow.Order
import Control.Arrow.Transformer.Kleisli
import Control.Category

import Data.Abstract.Except
import Data.Abstract.Widening (toJoin2)

import Data.Order(Complete)
import qualified Data.Order as O
import Data.Profunctor
import Data.Profunctor.Unsafe((.#))
import Data.Coerce

newtype ExceptT e c x y = ExceptT (KleisliT (Except e) c x y)
  deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowTrans, ArrowLift, ArrowRun,
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

instance (Complete e, ArrowJoin c, ArrowChoice c, ArrowApply c, Profunctor c) => ArrowApply (ExceptT e c) where
  app = lift (app .# first coerce)

type instance Fix x y (ExceptT e c) = ExceptT e (Fix (Dom (ExceptT e) x y) (Cod (ExceptT e) x y) c)
deriving instance (Complete e, ArrowJoin c, ArrowChoice c, ArrowFix (Dom (ExceptT e) x y) (Cod (ExceptT e) x y) c) => ArrowFix x y (ExceptT e c)

instance (Complete e, ArrowChoice c, ArrowJoin c) => ArrowJoin (ExceptT e c) where
  join lub f g = lift $ join (toJoin2 widening (O.⊔) lub) (unlift f) (unlift g)

deriving instance (Complete e, ArrowChoice c, ArrowJoin c, ArrowComplete (Except e y) c) => ArrowComplete y (ExceptT e c)


