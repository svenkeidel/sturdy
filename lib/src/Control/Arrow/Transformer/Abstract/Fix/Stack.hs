{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Transformer.Abstract.Fix.Stack(StackT,Stack,Monotone) where

import           Prelude hiding (pred,lookup,map,head,iterate,(.),elem)

import           Control.Category
import           Control.Arrow hiding (loop)
import           Control.Arrow.Strict
import           Control.Arrow.Fix.ControlFlow as ControlFlow
import           Control.Arrow.Fix.Cache as Cache
import           Control.Arrow.Fix.Stack (ArrowStack,ArrowStackDepth,ArrowStackElements)
import qualified Control.Arrow.Fix.Stack as Stack
import           Control.Arrow.Fix.Context (ArrowContext,ArrowJoinContext)
import           Control.Arrow.State
import           Control.Arrow.Trans
import           Control.Arrow.Order (ArrowLowerBounded)

import           Control.Arrow.Transformer.Reader

import           Data.Profunctor
import           Data.Profunctor.Unsafe ((.#))
import           Data.Coerce
import           Data.Empty
import           Data.Identifiable
import           Data.HashSet (HashSet)
import qualified Data.HashSet as Set
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import           Data.Order

newtype StackT stack a c x y = StackT (ReaderT (stack a) c x y)
  deriving (Profunctor,Category,Arrow,ArrowChoice,
            ArrowStrict,ArrowLift, ArrowLowerBounded z,
            ArrowParallelCache a b, ArrowIterateCache a b,
            ArrowState s,ArrowContext ctx, ArrowJoinContext u,
            ArrowControlFlow stmt)

runStackT :: (IsEmpty (stack a), Profunctor c) => StackT stack a c x y -> c x y
runStackT (StackT f) = lmap (\x -> (empty,x)) (runReaderT f)
{-# INLINE runStackT #-}

instance Profunctor c => ArrowTrans (StackT stack a c) where
  type Underlying (StackT stack a c) x y = c (stack a, x) y

instance (IsEmpty (stack a), ArrowRun c) => ArrowRun (StackT stack a c) where
  type Run (StackT stack a c) x y = Run c x y
  run f = run (runStackT f)
  {-# INLINE run #-}

instance (Profunctor c,ArrowApply c) => ArrowApply (StackT stack a c) where
  app = StackT (app .# first coerce)
  {-# INLINE app #-}

instance ArrowCache a b c => ArrowCache a b (StackT stack a c) where
  type Widening (StackT stack a c) = Widening c

-- Standard Stack -----------------------------------------------------------------------
data Stack a = Stack
  { elems :: HashSet a
  , stack :: [a]
  , depth  :: !Int
  }

instance IsEmpty (Stack a) where
  empty = Stack { elems = empty, stack = empty, depth = 0 }
  {-# INLINE empty #-}

instance (Identifiable a, Arrow c, Profunctor c) => ArrowStack a (StackT Stack a c) where
  push f = lift $ proc (st,(a,x)) -> do
    let st' = st { elems = Set.insert a (elems st)
                 , stack = a : stack st
                 , depth = depth st + 1
                 }
    unlift f -< (st', x)
  elem = lift $ proc (st,a) -> returnA -< Set.member a (elems st)
  {-# INLINE push #-}
  {-# INLINE elem #-}
  {-# SCC push #-}
  {-# SCC elem #-}

instance (Arrow c, Profunctor c) => ArrowStackDepth (StackT Stack a c) where
  depth = lift $ proc (st,()) -> returnA -< depth st
  {-# INLINE depth #-}

instance (Arrow c, Profunctor c) => ArrowStackElements a (StackT Stack a c) where
  peek = lift $ proc (st,()) -> returnA -< case stack st of [] -> Nothing; (x:_) -> Just x
  elems = lift $ proc (st,()) -> returnA -< stack st
  {-# INLINE peek #-}
  {-# INLINE elems #-}

-- Stack with a monotone component ------------------------------------------------------
data Monotone b where
  Monotone :: HashMap b a -> Monotone (a,b)

instance IsEmpty (Monotone (a,b)) where
  empty = Monotone empty
  {-# INLINE empty #-}

instance (PreOrd a, Identifiable b, Profunctor c, Arrow c) => ArrowStack (a,b) (StackT Monotone (a,b) c) where
  elem = lift $ arr $ \(Monotone m, (a,b)) -> Just a ⊑ M.lookup b m
  push f = lift $ lmap (\(Monotone m, ((a, b), x)) -> (Monotone (M.insert b a m), x)) (unlift f)
  {-# INLINE elem #-}
  {-# INLINE push #-}
  {-# SCC elem #-}
  {-# SCC push #-}
