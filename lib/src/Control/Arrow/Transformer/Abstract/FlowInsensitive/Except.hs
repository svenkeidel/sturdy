{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE GADTs #-}
module Control.Arrow.Transformer.Abstract.FlowInsensitive.Except(ExceptT,runExceptT) where

import Prelude hiding (id,lookup,(.),read,fail)

import Control.Category
import Control.Arrow
import Control.Arrow.Const
import Control.Arrow.Environment as Env
import Control.Arrow.Except
import Control.Arrow.Fail
import Control.Arrow.Fix
import Control.Arrow.Order hiding (bottom)
import Control.Arrow.Reader
import Control.Arrow.State
import Control.Arrow.Store as Store
import Control.Arrow.Trans
import Control.Arrow.Writer
import Control.Arrow.Transformer.Writer

import Data.Order hiding (lub)
import Data.OrderMonoid
import Data.Coerce
import Data.Profunctor
import Data.Profunctor.Unsafe

newtype ExceptT e c x y = ExceptT (WriterT (OrderMonoid e) c x y)
  deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowTrans, ArrowLift, ArrowRun, ArrowJoin,
            ArrowConst r, ArrowState s, ArrowReader r,
            ArrowEnv var val, ArrowClosure var val env, ArrowStore a b, ArrowFail err)

runExceptT :: Profunctor c => ExceptT e c x y -> c x (e,y)
runExceptT (ExceptT f) = coerce #. runWriterT f
{-# INLINE runExceptT #-}

instance (LowerBounded e, Complete e, ArrowJoin c) => ArrowExcept e (ExceptT e c) where
  type Join y (ExceptT e c) = (LowerBounded y, ArrowComplete (OrderMonoid e,y) c)
  throw = ExceptT $ proc e -> do
    tell -< coerce e
    returnA -< bottom
  try (ExceptT f) (ExceptT g) (ExceptT h) = ExceptT $ proc x -> do
    (exc, y) <- listen f -< x
    (g -< y) <⊔> (h -< (x,coerce exc))

instance (LowerBounded e, Complete e, ArrowApply c, Profunctor c) => ArrowApply (ExceptT e c) where
  app = lift (app .# first coerce)

type instance Fix x y (ExceptT e c) = ExceptT e (Fix (Dom (ExceptT e) x y) (Cod (ExceptT e) x y) c)
deriving instance (LowerBounded e, Complete e, ArrowJoin c, ArrowChoice c, ArrowFix (Dom (ExceptT e) x y) (Cod (ExceptT e) x y) c) => ArrowFix x y (ExceptT e c)

deriving instance (LowerBounded e, Complete e, ArrowComplete (OrderMonoid e, y) c) => ArrowComplete y (ExceptT e c)
instance (LowerBounded e, Complete e, ArrowEffectCommutative c) => ArrowEffectCommutative (ExceptT e c)

