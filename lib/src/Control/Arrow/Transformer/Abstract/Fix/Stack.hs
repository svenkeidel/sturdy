{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Transformer.Abstract.Fix.Stack(StackT,Stack,Proj2) where

import           Prelude hiding (pred,lookup,map,head,iterate,(.),elem)

import           Control.Category
import           Control.Arrow hiding (loop)
import           Control.Arrow.Fix.ControlFlow as ControlFlow
import           Control.Arrow.Fix.Cache as Cache
import           Control.Arrow.Fix.Stack (ArrowStack,ArrowStackDepth,ArrowStackElements,ArrowTopLevel)
import qualified Control.Arrow.Fix.Stack as Stack
import           Control.Arrow.Fix.Context (ArrowContext,ArrowJoinContext)
import           Control.Arrow.State
import           Control.Arrow.Trans
import           Control.Arrow.Order (ArrowJoin(..),ArrowComplete(..),ArrowLowerBounded,ArrowEffectCommutative)

import           Control.Arrow.Transformer.Reader

import           Data.Profunctor
import           Data.Profunctor.Unsafe ((.#))
import           Data.Coerce
import           Data.Empty
import           Data.HashSet (HashSet)
import qualified Data.HashSet as Set
import           Data.Identifiable

newtype StackT stack a c x y = StackT (ReaderT (stack a) c x y)
  deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowJoin,
            ArrowLowerBounded z, ArrowComplete z,
            ArrowCache a b, ArrowParallelCache a b, ArrowIterateCache,
            ArrowState s,ArrowContext ctx, ArrowJoinContext u,
            ArrowControlFlow stmt)

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

-- Standard Stack -----------------------------------------------------------------------
data Stack a = Stack
  { elems :: HashSet a
  , stack :: [a]
  , depth  :: !Int
  , isTopLevel :: Bool
  }

instance IsEmpty (Stack a) where
  empty = Stack { elems = empty, stack = empty, depth = 0, isTopLevel = True }
  {-# INLINE empty #-}

instance ArrowTrans (StackT Stack a c) where
  type Underlying (StackT Stack a c) x y = Underlying (ReaderT (Stack a) c) x y

instance (ArrowChoice c, Profunctor c) => ArrowTopLevel (StackT Stack a c) where
  topLevel combTop combLower f = lift $ proc (st,a) ->
    if isTopLevel st
    then unlift (combTop   f) -< (st {isTopLevel = False}, a)
    else unlift (combLower f) -< (st,a)
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

-- Second Projection -----------------------------------------------------------------------
data Proj2 stack b where
  Proj2 :: stack b -> Proj2 stack (a,b)

instance IsEmpty (stack b) => IsEmpty (Proj2 stack (a,b)) where
  empty = Proj2 empty
  {-# INLINE empty #-}

instance (Arrow c, Profunctor c, ArrowTopLevel (StackT stack b c)) => ArrowTopLevel (StackT (Proj2 stack) (a,b) c) where
  topLevel combTop combLower = lift1 $ Stack.topLevel (unlift1 combTop) (unlift1 combLower)
  {-# INLINE topLevel #-}

instance (ArrowApply c, Profunctor c, ArrowStack b (StackT stack b c)) => ArrowStack (a,b) (StackT (Proj2 stack) (a,b) c) where
  elem = lift (lmap snd Stack.elem)
  push f = lift $ proc (a,b) -> Stack.push (proc b' -> unlift f -< (a,b')) -<< b
  {-# INLINE elem #-}
  {-# INLINE push #-}

instance (Arrow c, Profunctor c, ArrowStackDepth (StackT stack b c)) => ArrowStackDepth (StackT (Proj2 stack) (a,b) c) where
  depth = lift Stack.depth
  {-# INLINE depth #-}

instance Profunctor c => ArrowTrans (StackT (Proj2 stack) (a,b) c) where
  type Underlying (StackT (Proj2 stack) (a,b) c) x y = StackT stack b c x y
  lift (StackT f) = StackT $ lift $ lmap (first (\(Proj2 st) -> st)) (unlift f)
  unlift (StackT f) = StackT $ lift $ lmap (first Proj2) (unlift f)
  {-# INLINE lift #-}
  {-# INLINE unlift #-}
