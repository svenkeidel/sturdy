{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Transformer.Abstract.MonotoneState where

import Prelude hiding (fail)
import Control.Category
import Control.Arrow
import Control.Arrow.Except as Except
import Control.Arrow.Fail as Fail
import Control.Arrow.Fix
import Control.Arrow.Order
import Control.Arrow.Trans
import Control.Arrow.State

import Control.Arrow.Transformer.State

import Data.Coerce
import Data.Order hiding (lub)
import Data.Profunctor
import Data.Profunctor.Unsafe

newtype MonotoneStateT s c x y = MonotoneStateT (StateT s c x y)
  deriving (Category,Profunctor,Arrow,ArrowChoice,ArrowTrans,ArrowLift,ArrowRun,ArrowEffectCommutative,ArrowState s)

instance (Complete y, ArrowEffectCommutative c) => ArrowComplete y (MonotoneStateT s c) where
  (MonotoneStateT f) <⊔> (MonotoneStateT g) = MonotoneStateT $ rmap (uncurry (⊔)) (f &&& g)

instance ArrowFail e c => ArrowFail e (MonotoneStateT s c) where
  type Join y (MonotoneStateT s c) = Fail.Join y c
  fail = lift $ second fail

instance ArrowExcept e c => ArrowExcept e (MonotoneStateT s c) where
  type Join y (MonotoneStateT s c) = (Except.Join y c, Except.Join (s,y) c)
  throw = lift $ second throw
  try (MonotoneStateT f) (MonotoneStateT g) (MonotoneStateT h) = MonotoneStateT $ try f g h

instance (ArrowEffectCommutative c) => ArrowJoin (MonotoneStateT s c) where
  join lub (MonotoneStateT f) (MonotoneStateT g) = MonotoneStateT $ rmap (uncurry lub) (f &&& g)

instance (ArrowApply c, Profunctor c) => ArrowApply (MonotoneStateT s c) where
  app = MonotoneStateT (app .# first coerce)

type instance Fix x y (MonotoneStateT s c) = MonotoneStateT s (Fix (Dom (MonotoneStateT s) x y) (Cod (MonotoneStateT s) x y) c)
deriving instance ArrowFix (Dom (MonotoneStateT s) x y) (Cod (MonotoneStateT s) x y) c => ArrowFix x y (MonotoneStateT s c)

