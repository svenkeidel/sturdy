{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Transformer.Concrete.Except(ExceptT,runExceptT) where

import Prelude hiding (id,(.))

import Control.Category
import Control.Arrow
import Control.Arrow.Const
import Control.Arrow.Environment as Env
import Control.Arrow.Closure as Cls
import Control.Arrow.Except
import Control.Arrow.Fail
import Control.Arrow.Fix
import Control.Arrow.Reader
import Control.Arrow.State
import Control.Arrow.Store as Store
import Control.Arrow.Trans

import Control.Arrow.Transformer.Kleisli

import Data.Concrete.Error
import Data.Profunctor
import Data.Profunctor.Unsafe((.#))
import Data.Coerce

-- | Arrow transformer that adds exceptions to the result of a computation
newtype ExceptT e c x y = ExceptT (KleisliT (Error e) c x y)
  deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowTrans,ArrowLift,ArrowRun,
            ArrowConst r,ArrowState s,ArrowReader r,ArrowFail err,
            ArrowEnv var val, ArrowClosure expr cls,ArrowStore var val)

runExceptT :: ExceptT e c x y -> c x (Error e y)
runExceptT = coerce
{-# INLINE runExceptT #-}

instance (ArrowChoice c, Profunctor c) => ArrowExcept e (ExceptT e c) where
  type Join y (ExceptT e c) = ()
  throw = lift $ arr Fail

  try f g h = lift $ proc x -> do
    e <- unlift f -< x
    case e of
      Success y -> unlift g -< y
      Fail er -> unlift h -< (x,er)

instance (ArrowChoice c, ArrowApply c, Profunctor c) => ArrowApply (ExceptT e c) where
  app = ExceptT $ app .# first coerce

type instance Fix (ExceptT e c) x y = ExceptT e (Fix c x (Error e y))
instance (ArrowFix (Underlying (ExceptT e c) x y)) => ArrowFix (ExceptT e c x y)
