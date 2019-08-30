{-# LANGUAGE Arrows #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Transformer.Abstract.Fix.Context where

import Prelude hiding (lookup,truncate)

import Control.Category
import Control.Arrow
import Control.Arrow.Fix
import Control.Arrow.Fix.Context
import Control.Arrow.Fix.Cache
import Control.Arrow.Trans
import Control.Arrow.Reader
import Control.Arrow.Order

import Control.Arrow.Transformer.Reader

import Data.Profunctor.Unsafe
import Data.Coerce
import Data.Empty
import Data.Abstract.CallString

callsiteSensitive :: (ArrowContext (CallString lab) c) => Int -> (a -> lab) -> IterationStrategy c a b
callsiteSensitive k getLabel f = proc a -> do
  callString <- askContext -< ()
  localContext f -< (truncate k (push (getLabel a) callString),a)
{-# INLINE callsiteSensitive #-}

newtype ContextT ctx c x y = ContextT (ReaderT ctx c x y)
  deriving (Category,Arrow,ArrowChoice,Profunctor,ArrowTrans,ArrowLift,ArrowComplete z,ArrowJoin,ArrowCache a b)

instance (Arrow c, Profunctor c) => ArrowContext ctx (ContextT ctx c) where
  askContext = ContextT ask
  localContext (ContextT f) = ContextT (local f)
  {-# INLINE askContext #-}
  {-# INLINE localContext #-}

runContextT :: (IsEmpty ctx, Profunctor c) => ContextT ctx c x y -> c x y
runContextT (ContextT f) = lmap (\x -> (empty,x)) (runReaderT f)
{-# INLINE runContextT #-}

instance (IsEmpty ctx, ArrowRun c) => ArrowRun (ContextT ctx c) where
  type Run (ContextT ctx c) x y = Run c x y
  run f = run (runContextT f)
  {-# INLINE run #-}

instance ArrowEffectCommutative c => ArrowEffectCommutative (ContextT ctx c)

instance (Profunctor c,ArrowApply c) => ArrowApply (ContextT ctx c) where
  app = ContextT (app .# first coerce)
  {-# INLINE app #-}
