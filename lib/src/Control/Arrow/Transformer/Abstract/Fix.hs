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
import           Control.Arrow.Order
import           Control.Arrow.Transformer.Const
import           Control.Arrow.Transformer.Static

import           Data.Identifiable
import           Data.Order
import           Data.Profunctor
import           Data.Profunctor.Unsafe((.#))
import           Data.Coerce

newtype FixT a b c x y = FixT { unFixT :: ConstT (IterationStrategy c a b) c x y }
  deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowComplete z, ArrowJoin)

-- runFixT :: (Identifiable a, PreOrd b, Profunctor c, ArrowRun t)
--   => IterationStrategy (t c) a b -> FixT a b (t c) x y -> c x y
-- runFixT iterationStrat f = run (runFixT' iterationStrat f)

runFixT :: (Identifiable a, PreOrd b)
  => IterationStrategy c a b -> FixT a b c x y -> c x y
runFixT iterationStrat (FixT f) = runConstT iterationStrat f
{-# INLINE runFixT #-}

instance ArrowRun c => ArrowRun (FixT a b c) where
  type Rep (FixT a b c) x y = IterationStrategy c a b -> Rep c x y
  run (FixT f) iterationStrat = run (runConstT iterationStrat f)
  {-# INLINE run #-}

type instance Fix x y (FixT _ _ c) = FixT x y c
instance (Profunctor c,ArrowChoice c,ArrowApply c) => ArrowFix a b (FixT a b c) where
  fix f = iterationStrategy (f (fix f))

instance (Profunctor c,ArrowApply c) => ArrowApply (FixT a b c) where
  app = FixT (app .# first coerce)

instance ArrowLift (FixT a b) where
  lift' = FixT . lift'

----- Helper functions -----
iterationStrategy :: FixT a b c a b -> FixT a b c a b
iterationStrategy (FixT (ConstT (StaticT f))) = FixT $ ConstT $ StaticT $ \strat -> strat (f strat)
