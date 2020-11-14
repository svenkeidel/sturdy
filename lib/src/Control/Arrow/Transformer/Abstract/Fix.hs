{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
module Control.Arrow.Transformer.Abstract.Fix(FixT(..),runFixT) where

import           Prelude hiding (id,(.),const,head,iterate,lookup)

import           Control.Category
import           Control.Arrow hiding (loop)
import           Control.Arrow.Primitive
import           Control.Arrow.Strict
import           Control.Arrow.Fix
import           Control.Arrow.Fix.Cache
import           Control.Arrow.Fix.CallCount
import           Control.Arrow.Fix.SCC
import           Control.Arrow.Fix.ControlFlow
import           Control.Arrow.Fix.Context
import           Control.Arrow.Fix.Metrics
import           Control.Arrow.Fix.Stack
import           Control.Arrow.Order(ArrowComplete(..),ArrowJoin(..))
import           Control.Arrow.Trans

import           Data.Profunctor
import           Data.Profunctor.Unsafe((.#))
import           Data.Coerce
import           Data.Order hiding (lub)

import           Control.Arrow.State

newtype FixT c x y = FixT (c x y)
  deriving (Profunctor,Category,Arrow,ArrowChoice,
            ArrowCallSite ctx, ArrowContext ctx a, ArrowControlFlow a,
            ArrowCache a b, ArrowParallelCache a b, ArrowIterateCache a b, ArrowGetCache cache,
            ArrowStack a,ArrowStackElements a,ArrowStackDepth, ArrowSCC a,
            ArrowMetrics a, ArrowStrict, ArrowPrimitive, ArrowCFG a, ArrowCallCount a)

runFixT :: FixT c x y -> c x y
runFixT (FixT f) = f
{-# INLINE runFixT #-}

instance ArrowRun c => ArrowRun (FixT c) where
  type Run (FixT c) x y = Run c x y

instance ArrowLift (FixT c) where
  type Underlying (FixT c) x y = c x y

instance ArrowFix (FixT c a b) where
  type Fix (FixT c a b) = FixT c a b
  fix = ?fixpointAlgorithm
  {-# INLINE fix #-}
  {-# SCC fix #-}

instance (Profunctor c,ArrowApply c) => ArrowApply (FixT c) where
  app = FixT (app .# first coerce)
  {-# INLINE app #-}

instance ArrowTrans FixT where
  lift' = FixT
  {-# INLINE lift' #-}

instance (Complete y, Profunctor c, Arrow c) => ArrowComplete y (FixT c) where
  FixT f <⊔> FixT g = FixT (rmap (uncurry (⊔)) (f &&& g))
  {-# INLINE (<⊔>) #-}

instance (Profunctor c, Arrow c) => ArrowJoin (FixT c) where
  joinSecond lub f (FixT g) = FixT (dimap (\x -> (x, x)) (\(x,y) -> (lub (f x) y)) (second g))

instance ArrowState s c => ArrowState s (FixT c) where
  get = lift get
  put = lift put
  {-# INLINE get #-}
  {-# INLINE put #-}

