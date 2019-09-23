{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Transformer.Abstract.Fix.Stack(StackT,Stack,widenInput,maxSize) where

import Prelude hiding (pred,lookup,map,head,iterate,(.))

import Control.Category
import Control.Arrow hiding (loop)
import Control.Arrow.Fix
import Control.Arrow.Fix.Reuse as Reuse
import Control.Arrow.Fix.Cache as Cache
import Control.Arrow.Fix.Stack(ArrowStack)
import qualified Control.Arrow.Fix.Stack as Stack
import Control.Arrow.Fix.Context(ArrowContext)
import Control.Arrow.State
import Control.Arrow.Trans
import Control.Arrow.Order(ArrowJoin(..),ArrowComplete(..),ArrowEffectCommutative)

import Control.Arrow.Transformer.Reader

import Data.Identifiable
import Data.Profunctor
import Data.Profunctor.Unsafe((.#))
import Data.Coerce
import Data.Empty
import Data.Order hiding (top)
import Data.Abstract.Widening (Widening)
import Data.HashSet(HashSet)
import qualified Data.HashSet as H
import Data.Abstract.Stable
import Data.Foldable

maxSize :: (ArrowChoice c, ArrowStack a c) => Int -> IterationStrategy c a b -> IterationStrategy c a b
maxSize limit strat f = proc a -> do
  n <- Stack.size -< ()
  if n < limit
  then f -< a
  else strat f -< a
{-# INLINE maxSize #-}

widenInput :: (Complete a, ArrowStack a c) => Widening a -> IterationStrategy c a b
widenInput widen f = proc a -> do
  m <- Stack.peek -< ()
  f -< case m of
    Nothing -> a
    Just x  -> snd $ x `widen` (x ⊔ a)
{-# INLINE widenInput #-}

data Stack a = Stack
  { top :: Maybe a
  , elems :: HashSet a
  , size :: Int
  }

instance IsEmpty (Stack a) where
  empty = Stack { top = Nothing, elems = H.empty, size = 0 }
  {-# INLINE empty #-}

newtype StackT stack a c x y = StackT (ReaderT (stack a) c x y)
  deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowJoin,ArrowComplete z,ArrowCache a b,ArrowState s,ArrowTrans,ArrowContext ctx a)

instance (PreOrd a, LowerBounded b, ArrowChoice c, ArrowReuse a b c, ArrowStack a (StackT stack a c)) => ArrowReuse a b (StackT stack a c) where
  reuse f = proc (a,s) -> case s of
    Unstable -> do
      m0 <- StackT (reuse f) -< (a,Unstable)
      stack <- Stack.elems -< ()
      returnA -< foldl' (\m a' -> if a ⊑ a' then m <> f a a' Unstable bottom else m) m0 stack
    Stable ->
      StackT (reuse f) -< (a,Stable)

instance (Identifiable a, Arrow c, Profunctor c) => ArrowStack a (StackT Stack a c) where
  peek = lift $ proc (stack,()) -> returnA -< top stack
  push f = lift $ proc (stack,a) -> unlift f -< (stack { top = Just a, elems = H.insert a (elems stack), size = size stack + 1 }, a)
  elem = lift $ proc (stack,a) -> returnA -< H.member a (elems stack)
  elems = lift $ proc (stack,()) -> returnA -< elems stack
  size = lift $ proc (stack,()) -> returnA -< size stack
  {-# INLINE peek #-}
  {-# INLINE push #-}
  {-# INLINE elem #-}
  {-# INLINE elems #-}
  {-# INLINE size #-}

runStackT :: (IsEmpty (stack a), Profunctor c) => StackT stack a c x y -> c x y
runStackT (StackT f) = lmap (\x -> (empty,x)) (runReaderT f)
{-# INLINE runStackT #-}

instance (IsEmpty (stack a), ArrowRun c) => ArrowRun (StackT stack a c) where
  type Run (StackT stack a c) x y = Run c x y
  run f = run (runStackT f)
  {-# INLINE run #-}

instance (Profunctor c,ArrowApply c) => ArrowApply (StackT cache a c) where
  app = StackT (app .# first coerce)
  {-# INLINE app #-}

instance ArrowEffectCommutative c => ArrowEffectCommutative (StackT stack a c)
