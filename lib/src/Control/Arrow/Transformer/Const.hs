{-# LANGUAGE Arrows #-}
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
import Control.Arrow.Environment
import Control.Arrow.Fail
import Control.Arrow.Except
import Control.Arrow.Fix
import Control.Arrow.Trans
import Control.Arrow.Reader
import Control.Arrow.State
import Control.Arrow.Store
import Control.Arrow.Const
import Control.Arrow.Writer
import Control.Arrow.Order

import Control.Arrow.Transformer.Static

import Data.Profunctor
import Data.Coerce

-- | Passes along constant data.
newtype ConstT r c x y = ConstT (StaticT ((->) r) c x y)
  deriving (Category,Profunctor,Arrow,ArrowChoice,ArrowComplete,ArrowLowerBounded,ArrowLift,
            ArrowState s,ArrowReader r',ArrowWriter w,
            ArrowEnv var val env, ArrowStore var val,
            ArrowFail e, ArrowExcept e)

runConstT :: r -> ConstT r c x y -> c x y
runConstT r (ConstT (StaticT f)) = f r

instance ArrowRun c => ArrowRun (ConstT r c) where
  type Rep (ConstT r c) x y = r -> Rep c x y
  run f r = run (runConstT r f)

instance (Arrow c, Profunctor c) => ArrowConst r (ConstT r c) where
  askConst f = ConstT $ StaticT $ \r -> runConstT r (f r)

type instance Fix x y (ConstT r c) = ConstT r (Fix x y c)
instance ArrowFix x y c => ArrowFix x y (ConstT r c) where
  fix f = ConstT $ StaticT $ \r -> fix (runConstT r . f . lift')

instance (ArrowApply c, Profunctor c) => ArrowApply (ConstT r c) where
  app = ConstT $ StaticT $ \r -> lmap (\(f,x) -> ((coerce f) r,x)) app
