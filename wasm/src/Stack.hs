{-# LANGUAGE Arrows #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Stack where

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Const
import           Control.Arrow.Except
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.Reader
import           Control.Arrow.State
import           Control.Arrow.Trans
import           Control.Arrow.Transformer.State
import           Control.Arrow.Utils

import           Data.Profunctor
import           Data.Coerce

import Numeric.Natural (Natural)

class ArrowStack v c | c -> v where
  push :: c v ()
  pop :: c () v
  peek :: c () v
  ifEmpty :: c x y -> c x y -> c x y
  localFreshStack :: c x y -> c x y

  pop2 :: ArrowChoice c => c () (v, v)
  pop2 = proc _ -> do
    v2 <- pop -< ()
    v1 <- pop -< ()
    returnA -< (v1, v2)

  popn :: ArrowChoice c => c Natural [v]
  popn = proc n -> case n of
    0 -> returnA -< []
    _ -> do
      v <- pop -< ()
      vs <- popn -< n-1
      returnA -< v:vs

  pushn :: ArrowChoice c => c [v] ()
  pushn = proc vs -> case vs of
    [] -> returnA -< ()
    v:vs' -> do
      pushn -< vs'
      push -< v

-- | Arrow transformer that adds a stack to a computation.
newtype StackT v c x y = StackT (StateT [v] c x y)
  deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowTrans,ArrowLift,ArrowRun,
            ArrowConst r, ArrowReader r, ArrowFail e, ArrowExcept e, ArrowState [v])

-- | Execute a computation and only return the result value and store.
runStackT :: StackT v c x y -> c ([v], x) ([v], y)
runStackT = coerce
{-# INLINE runStackT #-}

-- | Execute a computation and only return the result value.
evalStackT :: (Profunctor c) => StackT v c x y -> c ([v], x) y
evalStackT f = rmap pi2 (runStackT f)

-- | Execute a computation and only return the result store.
execStackT :: (Profunctor c) => StackT v c x y -> c ([v], x) [v]
execStackT f = rmap pi1 (runStackT f)

instance (ArrowChoice c, Profunctor c) => ArrowStack v (StackT v c) where
  push = StackT $ modify $ arr $ \(v,st) -> ((), v:st)
  pop = StackT $ modify $ arr $ \((),v:st) -> (v, st)
  peek = StackT $ get >>^ head
  ifEmpty (StackT f) (StackT g) = StackT $ proc x -> do
    st <- get -< ()
    case st of
      [] -> g -< x
      _ -> f -< x
  localFreshStack (StackT f) = StackT $ proc x -> do
    st <- get -< ()
    put -< []
    y <- f -< x
    put -< st
    returnA -< y
  pop2 = StackT $ modify $ arr $ \((),v2:v1:st) -> ((v1,v2), st)
  popn = StackT $ modify $ arr $ \(n,st) -> splitAt (fromIntegral n) st
  pushn = StackT $ modify $ arr $ \(st',st) -> ((),st'++st)

instance ArrowFix (Underlying (StackT v c) x y) => ArrowFix (StackT v c x y) where
    type Fix (StackT v c x y) = Fix (Underlying (StackT v c) x y)--StackT v (Fix c ([v],x) ([v],y))
