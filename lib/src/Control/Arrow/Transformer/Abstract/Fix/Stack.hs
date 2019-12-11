{-# LANGUAGE TupleSections #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Transformer.Abstract.Fix.Stack(StackT,Stack) where

import Prelude hiding (pred,lookup,map,head,iterate,(.))

import Control.Category
import Control.Arrow hiding (loop)
import Control.Arrow.Fix.Parallel as Parallel
import Control.Arrow.Fix.Cache as Cache
import Control.Arrow.Fix.Stack(ArrowStack)
import qualified Control.Arrow.Fix.Stack as Stack
import Control.Arrow.Fix.Context(ArrowContext,ArrowJoinContext)
import Control.Arrow.State
import Control.Arrow.Trans
import Control.Arrow.Order(ArrowJoin(..),ArrowComplete(..),ArrowEffectCommutative)

import Control.Arrow.Transformer.Reader

import Data.Profunctor
import Data.Profunctor.Unsafe((.#))
import Data.Coerce
import Data.Empty

newtype StackT stack a c x y = StackT (ReaderT (stack a) c x y)
  deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowJoin,ArrowComplete z,
            ArrowCache a b,ArrowState s,ArrowTrans,ArrowContext ctx, ArrowJoinContext u,
            ArrowParallel)

data Stack a = Stack
  { elems :: [a]
  , size  :: !Int
  }

instance IsEmpty (Stack a) where
  empty = Stack { elems = [], size = 0 }
  {-# INLINE empty #-}

instance (Arrow c, Profunctor c) => ArrowStack a (StackT Stack a c) where
  push f = lift $ proc (st,a) -> unlift f -< (st { elems = a : elems st, size = size st + 1 }, a)
  peek = lift $ proc (st,()) -> returnA -< case elems st of [] -> Nothing; (x:_) -> Just x
  elems = lift $ proc (st,()) -> returnA -< elems st
  size = lift $ proc (st,()) -> returnA -< size st
  {-# INLINE peek #-}
  {-# INLINE push #-}
  {-# INLINE size #-}

runStackT :: (IsEmpty (stack a), Profunctor c) => StackT stack a c x y -> c x y
runStackT (StackT f) = lmap (empty,) (runReaderT f)
{-# INLINE runStackT #-}

instance (IsEmpty (stack a), ArrowRun c) => ArrowRun (StackT stack a c) where
  type Run (StackT stack a c) x y = Run c x y
  run f = run (runStackT f)
  {-# INLINE run #-}

instance (Profunctor c,ArrowApply c) => ArrowApply (StackT stack a c) where
  app = StackT (app .# first coerce)
  {-# INLINE app #-}

instance ArrowEffectCommutative c => ArrowEffectCommutative (StackT stack a c)
