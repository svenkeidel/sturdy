{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module ValueT where

import           Prelude hiding ((.),fail)

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Const
import           Control.Arrow.Deduplicate
import           Control.Arrow.Except
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.Reader
import           Control.Arrow.State
import           Control.Arrow.Trans
import           Control.Arrow.Abstract.Join

import           Data.Order
import           Data.Profunctor
import           Data.Coerce

import           TermEnv

type instance Fix x y (ValueT t c) = ValueT t (Fix (Dom (ValueT t) x y) (Cod (ValueT t) x y) c)
newtype ValueT t c x y = ValueT { runValueT :: c x y }
  deriving (Category,Profunctor,Arrow,ArrowChoice,ArrowJoin,ArrowFail e,ArrowExcept e,ArrowFix a b,ArrowDeduplicate a b,ArrowReader r,IsTermEnv env t,ArrowConst r,ArrowState s)

instance ArrowTrans (ValueT t) where
  type Dom (ValueT t) x y = x
  type Cod (ValueT t) x y = y
  lift = ValueT
  unlift = runValueT

instance (Profunctor c, ArrowApply c) => ArrowApply (ValueT t c) where
  app = ValueT $ lmap (first coerce) app

deriving instance (PreOrd (c (Dom (ValueT t) x y) (Cod (ValueT t) x y))) => PreOrd (ValueT t c x y)
deriving instance (LowerBounded (c (Dom (ValueT t) x y) (Cod (ValueT t) x y))) => LowerBounded (ValueT t c x y)
deriving instance (Complete (c (Dom (ValueT t) x y) (Cod (ValueT t) x y))) => Complete (ValueT t c x y)
