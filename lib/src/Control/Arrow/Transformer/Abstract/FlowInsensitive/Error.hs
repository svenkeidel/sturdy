{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
module Control.Arrow.Transformer.Abstract.FlowInsensitive.Error(ErrorT,runErrorT) where

import Prelude hiding (id,(.),lookup,read)

import Control.Category
import Control.Arrow
import Control.Arrow.Const
import Control.Arrow.Environment as Env
import Control.Arrow.Fail
import Control.Arrow.Fix
import Control.Arrow.Trans
import Control.Arrow.Reader
import Control.Arrow.State
import Control.Arrow.Writer
import Control.Arrow.Store as Store
import Control.Arrow.Except as Exc
import Control.Arrow.Order hiding (bottom)
import Control.Arrow.Transformer.Writer

import Data.Order hiding (lub)
import Data.OrderMonoid
import Data.Profunctor
import Data.Profunctor.Unsafe
import Data.Coerce

-- | Describes computations that can fail.
newtype ErrorT e c x y = ErrorT (WriterT (OrderMonoid e) c x y)
  deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowTrans, ArrowLift, ArrowRun, 
            ArrowConst r, ArrowState s, ArrowReader r,
            ArrowEnv var val, ArrowClosure var val env, ArrowStore a b,
            ArrowExcept e',ArrowLowerBounded)

runErrorT :: Profunctor c => ErrorT e c x y -> c x (e,y)
runErrorT (ErrorT f) = coerce #. runWriterT f
{-# INLINE runErrorT #-}

instance (LowerBounded e, Complete e, Arrow c, Profunctor c) => ArrowFail e (ErrorT e c) where
  type Join y (ErrorT e c) = LowerBounded y
  fail = ErrorT $ proc e -> do
    tell -< coerce e
    returnA -< bottom

instance (LowerBounded e, Complete e, ArrowApply c, Profunctor c) => ArrowApply (ErrorT e c) where
  app = lift (app .# first coerce)

type instance Fix x y (ErrorT e c) = ErrorT e (Fix (Dom (ErrorT e) x y) (Cod (ErrorT e) x y) c)
deriving instance (LowerBounded e, Complete e, ArrowChoice c, ArrowFix (Dom (ErrorT e) x y) (Cod (ErrorT e) x y) c) =>
  ArrowFix x y (ErrorT e c)

deriving instance (LowerBounded e, Complete e, Arrow c,ArrowComplete (OrderMonoid e,y) c) => ArrowComplete y (ErrorT e c)
instance (LowerBounded e, Complete e, Arrow c,ArrowJoin c) => ArrowJoin (ErrorT e c) where
  join lub f g = lift $ join (\(e1,y1) (e2,y2) -> (e1 <> e2, lub y1 y2)) (unlift f) (unlift g)

instance (LowerBounded e, Complete e, ArrowEffectCommutative c) => ArrowEffectCommutative (ErrorT e c) where
