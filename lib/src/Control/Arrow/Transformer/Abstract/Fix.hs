{-# LANGUAGE Arrows #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
module Control.Arrow.Transformer.Abstract.Fix(FixT,runFixT) where

import           Prelude hiding (id,(.),const,head,iterate,lookup)

import           Control.Category
import           Control.Arrow hiding (loop)
import           Control.Arrow.Fix
import           Control.Arrow.Trans
import           Control.Arrow.Abstract.Join
import           Control.Arrow.Transformer.Const
import           Control.Arrow.Transformer.Static

import           Data.Identifiable
import           Data.Order
import           Data.Profunctor
import           Data.Coerce

import           Data.Abstract.IterationStrategy

newtype FixT a b c x y = FixT { unFixT :: ConstT (IterationStrategy c a b) c x y }
  deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowJoin)

runFixT :: (Identifiable a, PreOrd b, Profunctor c, ArrowRun t)
  => IterationStrategy (t c) a b -> FixT a b (t c) x y -> c x y
runFixT iterationStrat (FixT f) = run (runConstT (iterationStrat) f)

type instance Fix x y (FixT () () c) = FixT x y c
instance (Identifiable a, LowerBounded b, Profunctor c,ArrowChoice c,ArrowApply c) => ArrowFix a b (FixT a b c) where
  fix f = iterationStrategy (f (fix f))

instance (Profunctor c,ArrowApply c) => ArrowApply (FixT a b c) where app = FixT (lmap (first coerce) app)
deriving instance PreOrd (c x y) => PreOrd (FixT a b c x y)
deriving instance Complete (c x y) => Complete (FixT a b c x y)
deriving instance LowerBounded (c x y) => LowerBounded (FixT a b c x y)

instance ArrowLift (FixT a b) where
  lift' = FixT . lift'

----- Helper functions -----
iterationStrategy :: FixT a b c a b -> FixT a b c a b
iterationStrategy (FixT (ConstT (StaticT f))) = FixT $ ConstT $ StaticT $ \strat -> strat (f strat)
