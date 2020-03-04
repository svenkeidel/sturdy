{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
module Control.Arrow.Transformer.Abstract.Fix(FixT,runFixT) where

import           Prelude hiding (id,(.),const,head,iterate,lookup)

import           Control.Category
import           Control.Arrow hiding (loop)
import           Control.Arrow.Fix
import           Control.Arrow.Fix.Context (ArrowContext)
import           Control.Arrow.Order(ArrowEffectCommutative,ArrowComplete,ArrowJoin)
import           Control.Arrow.Trans
import           Control.Arrow.Transformer.Const
import           Control.Arrow.Transformer.Static

import           Data.Profunctor
import           Data.Profunctor.Unsafe((.#))
import           Data.Coerce


newtype FixT a b c x y = FixT { unFixT :: ConstT (FixpointAlgorithm c a b) c x y }
  deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowTrans,ArrowComplete z, ArrowJoin, ArrowContext ctx)

runFixT :: FixpointAlgorithm c a b -> FixT a b c x y -> c x y
runFixT comb (FixT f) = runConstT comb f
{-# INLINE runFixT #-}

instance ArrowRun c => ArrowRun (FixT a b c) where
  type Run (FixT a b c) x y = FixpointAlgorithm c a b -> Run c x y
  run (FixT f) comb = run (runConstT comb f)
  {-# INLINE run #-}

type instance Fix (FixT _ _ c) x y = FixT x y c
instance ArrowFix (FixT a b c a b) where
  fix f = lift $ \algorithm -> algorithm $ unliftConstT algorithm (unFixT . f . FixT)
  {-# NOINLINE fix #-}

instance (Profunctor c,ArrowApply c) => ArrowApply (FixT a b c) where
  app = FixT (app .# first coerce)
  {-# INLINE app #-}

instance ArrowLift (FixT a b) where
  lift' = FixT . lift'
  {-# INLINE lift' #-}



instance ArrowEffectCommutative c => ArrowEffectCommutative (FixT a b c)
