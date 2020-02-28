{-# LANGUAGE TupleSections #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Transformer.Abstract.Fix.Stack(StackT,Stack) where

import           Prelude hiding (pred,lookup,map,head,iterate,(.))

import           Control.Category
import           Control.Arrow hiding (loop)
import           Control.Arrow.Fix.ControlFlow as ControlFlow
import           Control.Arrow.Fix.Iterate as Iterate
import           Control.Arrow.Fix.Cache as Cache
import           Control.Arrow.Fix.Stack (ArrowStack,ArrowStackDepth,ArrowStackElements,ArrowTopLevel)
import qualified Control.Arrow.Fix.Stack as Stack
import           Control.Arrow.Fix.Context (ArrowContext,ArrowJoinContext)
import           Control.Arrow.State
import           Control.Arrow.Trans
import           Control.Arrow.Order (ArrowJoin(..),ArrowComplete(..),ArrowEffectCommutative)

import           Control.Arrow.Transformer.Reader

import           Data.Profunctor
import           Data.Profunctor.Unsafe ((.#))
import           Data.Coerce
import           Data.Empty
import           Data.HashSet (HashSet)
import qualified Data.HashSet as Set
import           Data.Identifiable

newtype StackT stack a c x y = StackT (ReaderT (stack a) c x y)
  deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowJoin,ArrowComplete z,
            ArrowCache a b,ArrowState s,ArrowTrans,ArrowContext ctx, ArrowJoinContext u,
            ArrowIterate,ArrowControlFlow stmt)

data Stack a = Stack
  { elems :: HashSet a
  , stack :: [a]
  , depth  :: !Int
  , isTopLevel :: Bool
  }

instance IsEmpty (Stack a) where
  empty = Stack { elems = empty, stack = empty, depth = 0, isTopLevel = True }
  {-# INLINE empty #-}

instance (ArrowChoice c, Profunctor c) => ArrowTopLevel (StackT Stack a c) where
  topLevel strat f = lift $ proc (st,a) ->
    if isTopLevel st
    then unlift (strat f) -< (st {isTopLevel = False}, a)
    else unlift f -< (st,a)
  {-# INLINABLE topLevel #-}

instance (Identifiable a, Arrow c, Profunctor c) => ArrowStack a (StackT Stack a c) where
  push f = lift $ proc (st,a) -> do
    let st' = st { elems = Set.insert a (elems st)
                 , stack = a : stack st
                 , depth = depth st + 1
                 }
    unlift f -< (st', a)
  elem = lift $ proc (st,a) -> returnA -< Set.member a (elems st)
  {-# INLINE push #-}
  {-# INLINE elem #-}

instance (Arrow c, Profunctor c) => ArrowStackDepth (StackT Stack a c) where
  depth = lift $ proc (st,()) -> returnA -< depth st
  {-# INLINE depth #-}

instance (Arrow c, Profunctor c) => ArrowStackElements a (StackT Stack a c) where
  peek = lift $ proc (st,()) -> returnA -< case stack st of [] -> Nothing; (x:_) -> Just x
  elems = lift $ proc (st,()) -> returnA -< stack st
  {-# INLINE peek #-}
  {-# INLINE elems #-}

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
