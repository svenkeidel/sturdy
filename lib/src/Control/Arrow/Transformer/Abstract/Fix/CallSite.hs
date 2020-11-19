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
module Control.Arrow.Transformer.Abstract.Fix.CallSite(CallSiteT) where

import           Prelude hiding (pred,lookup,map,head,iterate,(.),elem)

import           Control.Category
import           Control.Arrow hiding (loop)
import           Control.Arrow.Primitive
import           Control.Arrow.Strict
import           Control.Arrow.Fix.ControlFlow as ControlFlow
import           Control.Arrow.Fix.Cache as Cache
import           Control.Arrow.Fix.Stack (ArrowStack)
import           Control.Arrow.Fix.Context (ArrowContext, ArrowCallSite(..))
import           Control.Arrow.State
import           Control.Arrow.Trans
import           Control.Arrow.Order (ArrowLowerBounded)

import           Control.Arrow.Transformer.Reader

import           Data.Profunctor
import           Data.Profunctor.Unsafe ((.#))
import           Data.Coerce
import           Data.Empty
import           Data.Abstract.CallString as CallString

newtype CallSiteT label c x y = StackT (ReaderT (CallString label) c x y)
  deriving (Profunctor,Category,Arrow,ArrowChoice,
            ArrowStrict,ArrowTrans, ArrowLowerBounded z,
            ArrowParallelCache a b, ArrowIterateCache a b, ArrowGetCache cache,
            ArrowState s, ArrowStack a, ArrowContext ctx a',
            ArrowControlFlow stmt, ArrowPrimitive, ArrowCFG graph)

instance Profunctor c => ArrowLift (CallSiteT label c) where
  type Underlying (CallSiteT label c) x y = c (CallString label, x) y

instance (ArrowRun c) => ArrowRun (CallSiteT label c) where
  type Run (CallSiteT label c) x y = Run c x y
  run f = run (lmap (\x -> (empty,x)) (unlift f))
  {-# INLINE run #-}

instance (Profunctor c,ArrowApply c) => ArrowApply (CallSiteT label c) where
  app = StackT (app .# first coerce)
  {-# INLINE app #-}

instance ArrowCache a b c => ArrowCache a b (CallSiteT label c) where
  type Widening (CallSiteT label c) = Widening c

instance (Arrow c, Profunctor c) => ArrowCallSite label (CallSiteT label c) where
  getCallSite = lift $ arr (\(callString, _) -> callString)
  {-# INLINE getCallSite #-}

  pushLabel k f = lift $ proc (callString, (label,x)) -> do
    let callString' = CallString.truncate k (CallString.push label callString)
    unlift f -< (callString',x)
  {-# INLINE pushLabel #-}
