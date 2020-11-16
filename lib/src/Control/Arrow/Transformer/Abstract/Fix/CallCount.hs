{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Transformer.Abstract.Fix.CallCount(CallCountT) where

import           Prelude hiding (pred,lookup,map,head,iterate,(.),elem)

import           Control.Category
import           Control.Arrow hiding (loop)
import           Control.Arrow.Primitive
import           Control.Arrow.Strict
import           Control.Arrow.Fix.ControlFlow as ControlFlow
import           Control.Arrow.Fix.Cache as Cache
import           Control.Arrow.Fix.Stack (ArrowStack)
import           Control.Arrow.Fix.Context (ArrowContext)
import           Control.Arrow.Fix.CallCount
import           Control.Arrow.State
import           Control.Arrow.Trans
import           Control.Arrow.Order (ArrowLowerBounded)

import           Control.Arrow.Transformer.Reader

import           Data.Profunctor
import           Data.Profunctor.Unsafe ((.#))
import           Data.Coerce
import           Data.Identifiable
import           Data.Empty
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as Map

newtype CallCountT label c x y = StackT (ReaderT (HashMap label Int) c x y)
  deriving (Profunctor,Category,Arrow,ArrowChoice,
            ArrowStrict,ArrowTrans, ArrowLowerBounded z,
            ArrowParallelCache a b, ArrowIterateCache a b, ArrowGetCache cache,
            ArrowState s, ArrowStack a, ArrowContext ctx a',
            ArrowControlFlow stmt, ArrowPrimitive, ArrowCFG graph)

instance Profunctor c => ArrowLift (CallCountT label c) where
  type Underlying (CallCountT label c) x y = c (HashMap label Int, x) y

instance (ArrowRun c) => ArrowRun (CallCountT label c) where
  type Run (CallCountT label c) x y = Run c x y
  run f = run (lmap (\x -> (empty,x)) (unlift f))
  {-# INLINE run #-}

instance (Profunctor c,ArrowApply c) => ArrowApply (CallCountT label c) where
  app = StackT (app .# first coerce)
  {-# INLINE app #-}

instance ArrowCache a b c => ArrowCache a b (CallCountT label c) where
  type Widening (CallCountT label c) = Widening c

instance (Identifiable label, Arrow c, Profunctor c) => ArrowCallCount label (CallCountT label c) where
  getCallCount = lift $ arr (\(callCount, label) -> Map.lookupDefault 0 label callCount)
  {-# INLINE getCallCount #-}

  incrementCallCount f = lift $ proc (callCount, (label,x)) -> do
    let callCount' = Map.insertWith (\_ old -> old + 1) label 1 callCount
    unlift f -< (callCount', x)
  {-# INLINE incrementCallCount #-}
