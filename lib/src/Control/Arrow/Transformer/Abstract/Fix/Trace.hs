{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Arrow.Transformer.Abstract.Fix.Trace where

import Prelude hiding (pred,lookup,map,head,iterate,(.),truncate)

import Control.Category
import Control.Arrow hiding ((<+>))
import Control.Arrow.Fix.Chaotic
import Control.Arrow.Fix.ControlFlow as CF
import Control.Arrow.Fix.Cache as Cache
import Control.Arrow.Fix.Stack as Stack
import Control.Arrow.Fix.Context as Context
import Control.Arrow.Fix.Metrics as Metrics
import Control.Arrow.State
import Control.Arrow.Trans
import Control.Arrow.Order

import Data.Profunctor.Unsafe
import Data.Coerce
import Data.Abstract.Stable
import Debug.Trace as Debug
import Data.Text.Prettyprint.Doc

newtype TraceT c x y = TraceT (c x y)
  deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowTrans,ArrowComplete z,ArrowJoin,
            ArrowEffectCommutative,ArrowComponent a,ArrowStack a,ArrowContext ctx,ArrowState s,ArrowControlFlow stmt,
            ArrowTopLevel,ArrowStackDepth,ArrowStackElements a, ArrowFiltered a,
            ArrowLowerBounded b)

instance (Pretty a, Pretty b, ArrowCache a b c) => ArrowCache a b (TraceT c) where
  initialize = TraceT $ proc a -> do
    b  <- initialize -< a
    returnA -< Debug.trace (show (vsep ["INITIALIZE", "x:" <+> pretty a, "y:" <+> pretty b] <> line)) b
  lookup = TraceT $ proc a -> do
    b  <- lookup -< a
    returnA -< Debug.trace (show (vsep ["LOOKUP", "x:" <+> pretty a, "y:" <+> pretty b] <> line)) b
  update = TraceT $ proc (a,b) -> do
    bOld  <- lookup -< a
    (s,b') <- update -< (a,b)
    returnA -< Debug.trace (show (vsep ["UPDATE", "x:" <+> pretty a, "y:" <+> pretty bOld <+> "⊔" <+> pretty b <+> showArrow s <+> pretty b'] <> line)) (s,b')
  write = TraceT $ proc (a,b,s) -> do
    bOld  <- lookup -< a
    write -< Debug.trace (show (vsep ["WRITE", "x:" <+> pretty a, "y:" <+> pretty bOld <+> showArrow s <+> pretty b] <> line)) (a,b,s)
  setStable = TraceT $ proc (s,a) ->
    setStable -< Debug.trace (show (vsep ["SET STABLE", "x:" <+> pretty a, pretty s] <> line)) (s,a)
  {-# INLINE lookup #-}
  {-# INLINE update #-}
  {-# INLINE write #-}
  {-# INLINE setStable #-}

instance (Pretty a, Pretty b, ArrowParallelCache a b c) => ArrowParallelCache a b (TraceT c) where
  lookupOldCache = TraceT $ proc a -> do
    b  <- lookupOldCache -< a
    returnA -< Debug.trace (show (vsep ["LOOKUP_OLD", "x:" <+> pretty a, "y:" <+> pretty b] <> line)) b
  lookupNewCache = TraceT $ proc a -> do
    b  <- lookupNewCache -< a
    returnA -< Debug.trace (show (vsep ["LOOKUP_NEW", "x:" <+> pretty a, "y:" <+> pretty b] <> line)) b
  updateNewCache = TraceT $ proc (a,b) -> do
    bOld  <- lookupNewCache -< a
    (s,b') <- updateNewCache -< (a,b)
    returnA -< Debug.trace (show (vsep ["UPDATE_NEW", "x:" <+> pretty a, "y:" <+> pretty bOld <+> "∇" <+> pretty b <+> showArrow s <+> pretty b'] <> line)) (s,b')
  isStable = TraceT $ proc () -> do
    stable  <- isStable -< ()
    returnA -< Debug.trace (show ("IS_STABLE:" <+> pretty stable <> line)) stable
  {-# INLINE lookupOldCache #-}
  {-# INLINE lookupNewCache #-}
  {-# INLINE updateNewCache #-}
  {-# INLINE isStable #-}

instance ArrowIterateCache c => ArrowIterateCache (TraceT c) where
  nextIteration = TraceT $ proc () ->
    nextIteration -< Debug.trace (show ("NEXT_ITERATION" <> line)) ()
  {-# INLINE nextIteration #-}

runTraceT :: TraceT c x y -> c x y
runTraceT (TraceT f) = f
{-# INLINE runTraceT #-}

instance ArrowRun c => ArrowRun (TraceT c) where
  type Run (TraceT c) x y = Run c x y
  run f = run (runTraceT f)

instance ArrowLift TraceT where
  lift' = coerce
  {-# INLINE lift' #-}

instance (Profunctor c,ArrowApply c) => ArrowApply (TraceT c) where
  app = TraceT (app .# first coerce)
  {-# INLINE app #-}

