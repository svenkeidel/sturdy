{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Transformer.Abstract.Fix.Context where

import Prelude hiding (lookup,truncate,(.),id)

import Control.Category
import Control.Arrow
import Control.Arrow.Fix.Context
import Control.Arrow.Fix.Cache
import Control.Arrow.Trans
import Control.Arrow.Reader
import Control.Arrow.Order

import Control.Arrow.Transformer.Reader

import Data.Profunctor.Unsafe
import Data.Coerce
import Data.Empty
import Data.Order hiding (lub)

newtype ContextT ctx c x y = ContextT (ReaderT ctx c x y)
  deriving (Category,Arrow,ArrowChoice,Profunctor,ArrowTrans,ArrowCache u b)

runContextT :: (IsEmpty ctx, Profunctor c) => ContextT ctx c x y -> c x y
runContextT (ContextT f) = lmap (empty,) (runReaderT f)
{-# INLINE runContextT #-}

instance (Arrow c, Profunctor c) => ArrowContext ctx (ContextT ctx c) where
  askContext = ContextT ask
  localContext (ContextT f) = ContextT (local f)
  {-# INLINE askContext #-}
  {-# INLINE localContext #-}

instance ArrowLift (ContextT ctx) where
  lift' = ContextT . lift'
  {-# INLINE lift' #-}

instance (IsEmpty ctx, ArrowRun c) => ArrowRun (ContextT ctx c) where
  type Run (ContextT ctx c) x y = Run c x y
  run f = run (runContextT f)
  {-# INLINE run #-}

instance (Complete y, ArrowEffectCommutative c) => ArrowComplete y (ContextT ctx c) where
  ContextT f <⊔> ContextT g = ContextT $ rmap (uncurry (⊔)) (f &&& g)
  {-# INLINE (<⊔>) #-}

instance (Arrow c, Profunctor c) => ArrowJoin (ContextT ctx c) where
  joinSecond lub f (ContextT g) = ContextT (rmap (\(x,y) -> f x `lub` y) (id &&& g))
  {-# INLINE joinSecond #-}

instance ArrowEffectCommutative c => ArrowEffectCommutative (ContextT ctx c)

instance (Profunctor c,ArrowApply c) => ArrowApply (ContextT ctx c) where
  app = ContextT (app .# first coerce)
  {-# INLINE app #-}
