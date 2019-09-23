{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Transformer.Abstract.Fix.Context where

import Prelude hiding (lookup,truncate,(.))

import Control.Category
import Control.Arrow
import Control.Arrow.Fix
import Control.Arrow.Fix.Context
import Control.Arrow.Fix.Cache
import Control.Arrow.Trans
import Control.Arrow.Reader
import Control.Arrow.State
import Control.Arrow.Order

import Control.Arrow.Transformer.Reader
import Control.Arrow.Transformer.State

import Data.Abstract.CallString
import qualified Data.Abstract.Widening as W

import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import Data.Profunctor.Unsafe
import Data.Coerce
import Data.Empty
import Data.Order
import Data.Identifiable

callsiteSensitive :: forall a lab b c. ArrowContext (CallString lab) a c => Int -> (a -> lab) -> Widening c a -> IterationStrategy c a b
callsiteSensitive k getLabel = callsiteSensitive' k (Just . getLabel)
{-# INLINE callsiteSensitive #-}

callsiteSensitive' :: forall a lab b c. ArrowContext (CallString lab) a c => Int -> (a -> Maybe lab) -> Widening c a -> IterationStrategy c a b
callsiteSensitive' k getLabel widen = recordCallsite k getLabel . joinByContext' widen
{-# INLINE callsiteSensitive' #-}

recordCallsite :: forall a lab b c. ArrowContext (CallString lab) a c => Int -> (a -> Maybe lab) -> IterationStrategy c a b
recordCallsite k getLabel f = proc a -> do
  callString <- askContext -< ()
  let callString' = case getLabel a of
        Just lab -> truncate k (push lab callString)
        Nothing -> callString
  localContext f -< (callString',a)
{-# INLINE recordCallsite #-}

newtype ContextT ctx a c x y = ContextT (ReaderT ctx (StateT (HashMap ctx a) c) x y)
  deriving (Category,Arrow,ArrowChoice,Profunctor,ArrowTrans,ArrowCache a b)

instance (Identifiable ctx, PreOrd a, ArrowChoice c, Profunctor c) => ArrowContext ctx a (ContextT ctx a c) where
  type Widening (ContextT ctx a c) a = W.Widening a
  askContext = ContextT ask
  localContext (ContextT f) = ContextT (local f)
  joinByContext widen = ContextT $ proc a -> do
    ctx <- ask -< ()
    cache <- get -< ()
    case M.lookup ctx cache of
      -- If there exists a stable cached entry and the actual input is
      -- smaller than the cached input, recurse the cached input.
      Just a'
        | a ⊑ a' -> returnA -< a'
        | otherwise -> do
          -- If there exists the actual input is not smaller than the cached
          -- input, widen the input.
          let (_,a'') = widen a' a
          put -< M.insert ctx a'' cache
          returnA -< a''
      Nothing -> do
        put -< M.insert ctx a cache
        returnA -< a
  {-# INLINE askContext #-}
  {-# INLINE localContext #-}
  {-# INLINE joinByContext #-}

runContextT :: (IsEmpty ctx, Profunctor c) => ContextT ctx a c x y -> c x y
runContextT (ContextT f) = dimap (\x -> (empty,(empty,x))) snd (runStateT (runReaderT f))
{-# INLINE runContextT #-}

instance ArrowLift (ContextT ctx a) where
  lift' = ContextT . lift' . lift'
  {-# INLINE lift' #-}

instance (IsEmpty ctx, ArrowRun c) => ArrowRun (ContextT ctx a c) where
  type Run (ContextT ctx a c) x y = Run c x y
  run f = run (runContextT f)
  {-# INLINE run #-}

instance (Complete y, ArrowEffectCommutative c) => ArrowComplete y (ContextT ctx a c) where
  ContextT f <⊔> ContextT g = ContextT $ rmap (uncurry (⊔)) (f &&& g)
  {-# INLINE (<⊔>) #-}

instance (Arrow c, Profunctor c) => ArrowJoin (ContextT ctx a c) where
  joinSecond (ContextT f) = ContextT (second f)
  {-# INLINE joinSecond #-}

instance ArrowEffectCommutative c => ArrowEffectCommutative (ContextT ctx a c)

instance (Profunctor c,ArrowApply c) => ArrowApply (ContextT ctx a c) where
  app = ContextT (app .# first coerce)
  {-# INLINE app #-}
