{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Transformer.Abstract.Fix.Trace where

import Prelude hiding (pred,lookup,map,head,iterate,(.),truncate,log)

import Control.Category
import Control.Arrow hiding ((<+>))
import Control.Arrow.Fix.SCC
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
import Prettyprinter

newtype TraceT c x y = TraceT (c x y)
  deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowLift, ArrowLowerBounded b,
            ArrowSCC a, ArrowState s, ArrowControlFlow stmt,
            ArrowTopLevel,ArrowStackDepth,ArrowStackElements a, ArrowMetrics a)

log :: Arrow c => c (Doc ann) ()
log = proc s -> do
  returnA -< Debug.trace (show s) ()

instance (Pretty a, ArrowStack a c) => ArrowStack a (TraceT c) where
  elem = TraceT $ proc a -> do
    () <- log -< pretty a <> line
    Stack.elem -< a

  push (TraceT f) = TraceT (push f)

-- instance ArrowCache a b c => ArrowCache a b (TraceT c) where
--   type Widening (TraceT c) = Cache.Widening c

instance (Pretty a, Pretty b, ArrowCache a b c) => ArrowCache a b (TraceT c) where
  type Widening (TraceT c) = Cache.Widening c
  initialize = TraceT $ proc a -> do
    b  <- initialize -< a
    () <- log -< vsep ["INITIALIZE", "x:" <+> pretty a, "y:" <+> pretty b] <> line
    returnA -< b
  lookup = TraceT $ proc a -> do
    b  <- lookup -< a
    () <- log -< vsep ["LOOKUP", "x:" <+> pretty a, "y:" <+> pretty b] <> line
    returnA -< b
  update = TraceT $ proc (st, a,b) -> do
    bOld  <- lookup -< a
    (s,a',b') <- update -< (st, a,b)
    () <- log -< vsep ["UPDATE", "x:" <+> pretty a <+> "->" <+> pretty a', "y:" <+> pretty bOld <+> "⊔" <+> pretty b <+> showArrow s <+> pretty b'] <> line
    returnA -< (s,a',b')
  write = TraceT $ proc (a,b,s) -> do
    bOld  <- lookup -< a
    () <- log -< vsep ["WRITE", "x:" <+> pretty a, "y:" <+> pretty bOld <+> showArrow s <+> pretty b] <> line
    write -< (a,b,s)
  setStable = TraceT $ proc (s,a) -> do
    () <- log -< vsep ["SET STABLE", "x:" <+> pretty a, pretty s] <> line
    setStable -< (s,a)
  {-# INLINE lookup #-}
  {-# INLINE update #-}
  {-# INLINE write #-}
  {-# INLINE setStable #-}

instance (Pretty ctx, ArrowCallSite ctx c) => ArrowCallSite ctx (TraceT c) where
  getCallSite = TraceT $ proc () -> do
    ctx <- getCallSite -< ()
    () <- log -< "CONTEXT: " <> pretty ctx <> line
    returnA -< ctx

  pushLabel k (TraceT f) = TraceT $ pushLabel k f

instance (Pretty ctx, Pretty a, ArrowContext ctx a c) => ArrowContext ctx a (TraceT c) where
  type Widening (TraceT c) = Context.Widening c
  joinByContext = TraceT $ proc (ctx,a) -> do
    aNew <- joinByContext -< (ctx,a)
    () <- log -< vsep ["WIDEN", "ctx:" <+> pretty ctx, "aOld:" <+> pretty a, "aNew:" <+> pretty aNew] <> line
    returnA -< aNew

instance (Pretty a, Pretty b, ArrowParallelCache a b c) => ArrowParallelCache a b (TraceT c) where
  lookupOldCache = TraceT $ proc a -> do
    b  <- lookupOldCache -< a
    () <- log -< vsep ["LOOKUP_OLD", "x:" <+> pretty a, "y:" <+> pretty b] <> line
    returnA -< b
  lookupNewCache = TraceT $ proc a -> do
    b  <- lookupNewCache -< a
    () <- log -< vsep ["LOOKUP_NEW", "x:" <+> pretty a, "y:" <+> pretty b] <> line
    returnA -<  b
  updateNewCache = TraceT $ proc (a,b) -> do
    bOld  <- lookupNewCache -< a
    b' <- updateNewCache -< (a,b)
    () <- log -< vsep ["UPDATE_NEW", "x:" <+> align (pretty a), "y:" <+> align (pretty bOld <+> "∇" <+> pretty b <+> "->" <+> pretty b')] <> line
    returnA -<  b'
  isStable = TraceT $ proc () -> do
    stable  <- isStable -< ()
    () <- log -< "IS_STABLE:" <+> pretty stable <> line
    returnA -< stable
  {-# INLINE lookupOldCache #-}
  {-# INLINE lookupNewCache #-}
  {-# INLINE updateNewCache #-}
  {-# INLINE isStable #-}

instance (Pretty a, Pretty b, ArrowIterateCache a b c) => ArrowIterateCache a b (TraceT c) where
  nextIteration = TraceT $ proc x -> do
    x' <- nextIteration -< x
    () <- log -< "NEXT_ITERATION:" <> line <> pretty x <> line <> pretty x' <> line
    returnA -< x'
  {-# INLINE nextIteration #-}

instance ArrowRun c => ArrowRun (TraceT c) where
  type Run (TraceT c) x y = Run c x y
  run (TraceT f) = run f

instance ArrowTrans TraceT where
  lift' = coerce
  {-# INLINE lift' #-}

instance (Profunctor c,ArrowApply c) => ArrowApply (TraceT c) where
  app = TraceT (app .# first coerce)
  {-# INLINE app #-}

