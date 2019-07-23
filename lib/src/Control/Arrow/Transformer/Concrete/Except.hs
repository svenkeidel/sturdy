{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Transformer.Concrete.Except(ExceptT,runExceptT) where

import Prelude hiding (id,(.))

import Control.Arrow
import Control.Arrow.Const
import Control.Arrow.Environment as Env
import Control.Arrow.Fail
import Control.Arrow.Fix
import Control.Arrow.Trans
import Control.Arrow.Reader
import Control.Arrow.Store as Store
import Control.Arrow.State
import Control.Arrow.Except
import Control.Category
import Control.Arrow.Transformer.Kleisli

import Data.Concrete.Error
import Data.Profunctor
import Data.Profunctor.Unsafe((.#))
import Data.Coerce

-- | Arrow transformer that adds exceptions to the result of a computation
newtype ExceptT e c x y = ExceptT (KleisliT (Error e) c x y) 
  deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowTrans,ArrowLift,
            ArrowState s,ArrowReader r,ArrowFail err,ArrowEnv a b env,ArrowStore var val,ArrowConst r,
            ArrowRun)

runExceptT :: ExceptT e c x y -> c x (Error e y)
runExceptT = coerce
{-# INLINE runExceptT #-}

instance (ArrowChoice c, Profunctor c) => ArrowExcept e (ExceptT e c) where
  type Join (ExceptT e c) x y = ()

  throw = lift $ arr Fail

  try f g h = lift $ proc x -> do
    e <- unlift f -< x
    case e of
      Success y -> unlift g -< y
      Fail er -> unlift h -< (x,er)

instance (ArrowChoice c, ArrowApply c, Profunctor c) => ArrowApply (ExceptT e c) where
  app = ExceptT $ app .# first coerce

type instance Fix x y (ExceptT e c) = ExceptT e (Fix (Dom (ExceptT e) x y) (Cod (ExceptT e) x y) c)
deriving instance (ArrowChoice c, ArrowFix (Dom (ExceptT e) x y) (Cod (ExceptT e) x y) c) => ArrowFix x y (ExceptT e c)
