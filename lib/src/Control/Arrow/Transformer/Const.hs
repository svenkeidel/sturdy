{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
module Control.Arrow.Transformer.Const where

import Prelude hiding (id,(.),lookup,read)

import Control.Category

import Control.Arrow
import Control.Arrow.Const
import Control.Arrow.Fix.Context
import Control.Arrow.Environment
import Control.Arrow.Except
import Control.Arrow.Fail
import Control.Arrow.Fix
import Control.Arrow.Order
import Control.Arrow.Reader
import Control.Arrow.State
import Control.Arrow.Store
import Control.Arrow.Trans
import Control.Arrow.Writer

import Control.Arrow.Transformer.Static

import Data.Profunctor
import Data.Coerce

-- | Passes along constant data.
newtype ConstT r c x y = ConstT (StaticT ((->) r) c x y)
  deriving (Category,Profunctor,Arrow,ArrowChoice,ArrowLowerBounded,ArrowLift,ArrowJoin,
            ArrowState s,ArrowReader r',ArrowWriter w,
            ArrowEnv var val, ArrowClosure var val env, ArrowStore var val,
            ArrowFail e, ArrowExcept e,ArrowContext ctx a)

constT :: (r -> c x y) -> ConstT r c x y
constT f = ConstT (StaticT f)
{-# INLINE constT #-}

runConstT :: r -> ConstT r c x y -> c x y
runConstT r (ConstT (StaticT f)) = f r
{-# INLINE runConstT #-}

liftConstT :: (c x y -> c' x' y') -> ConstT r c x y -> ConstT r c' x' y'
liftConstT f g = lift $ \r -> f (unlift g r)
{-# INLINE liftConstT #-}

instance ArrowTrans (ConstT r c) where
  type Underlying (ConstT r c) x y = r -> c x y

instance ArrowRun c => ArrowRun (ConstT r c) where
  type Run (ConstT r c) x y = r -> Run c x y
  run f r = run (runConstT r f)
  {-# INLINE run #-}

instance (Arrow c, Profunctor c) => ArrowConst r (ConstT r c) where
  askConst f = ConstT $ StaticT $ \r -> runConstT r (f r)
  {-# INLINE askConst #-}

instance (Arrow c, Profunctor c, ArrowFix (c x y)) => ArrowFix (ConstT r c x y) where
  fix f = ConstT $ StaticT $ \r -> fix (runConstT r . f . lift')
  {-# INLINE fix #-}

instance (ArrowApply c, Profunctor c) => ArrowApply (ConstT r c) where
  app = ConstT $ StaticT $ \r -> lmap (\(f,x) -> (coerce f r,x)) app
  {-# INLINE app #-}

deriving instance ArrowComplete y c =>  ArrowComplete y (ConstT r c)

type instance Fix (ConstT r c) x y = ConstT r (Fix c x y)
