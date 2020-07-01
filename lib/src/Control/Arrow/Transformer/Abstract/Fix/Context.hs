{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Transformer.Abstract.Fix.Context where

import Prelude hiding (lookup,truncate,(.),id)

import Control.Category
import Control.Arrow
import Control.Arrow.Primitive
import Control.Arrow.Strict
import Control.Arrow.Fix.ControlFlow
import Control.Arrow.Fix.Context
import Control.Arrow.Fix.Cache as Cache
import Control.Arrow.Trans
import Control.Arrow.Reader
import Control.Arrow.Fix.GarbageCollection

import Control.Arrow.Transformer.Reader

import Data.Profunctor.Unsafe
import Data.Coerce
import Data.Empty

newtype ContextT ctx c x y = ContextT (ReaderT ctx c x y)
  deriving (Category,Profunctor,Arrow,ArrowChoice,ArrowStrict,
            ArrowLift,ArrowControlFlow stmt, ArrowPrimitive, ArrowGarbageCollection addr)

runContextT :: (IsEmpty ctx, Profunctor c) => ContextT ctx c x y -> c x y
runContextT (ContextT f) = lmap (empty,) (runReaderT f)
{-# INLINE runContextT #-}

instance (Arrow c, Profunctor c) => ArrowContext ctx (ContextT ctx c) where
  askContext = ContextT ask
  localContext (ContextT f) = ContextT (local f)
  {-# INLINE askContext #-}
  {-# INLINE localContext #-}

instance ArrowTrans (ContextT ctx) where
  lift' = ContextT . lift'
  {-# INLINE lift' #-}

instance (IsEmpty ctx, ArrowRun c) => ArrowRun (ContextT ctx c) where
  type Run (ContextT ctx c) x y = Run c x y
  run f = run (runContextT f)
  {-# INLINE run #-}

instance ArrowCache a b c => ArrowCache a b (ContextT ctx c) where
  type Widening (ContextT ctx c) = Cache.Widening c

instance (Profunctor c,ArrowApply c) => ArrowApply (ContextT ctx c) where
  app = ContextT (app .# first coerce)
  {-# INLINE app #-}
