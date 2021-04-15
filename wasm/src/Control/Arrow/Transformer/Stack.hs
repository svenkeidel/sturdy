{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Transformer.Stack where

import           Data

import           Control.Category hiding (id)
import           Control.Arrow
import           Control.Arrow.Const
import           Control.Arrow.Except
import           Control.Arrow.Fail
import           Control.Arrow.Fix
--import           Control.Arrow.Logger
import           Control.Arrow.MemAddress
import           Control.Arrow.Memory
import           Control.Arrow.MemSizable
import           Control.Arrow.Order
import           Control.Arrow.Reader
import           Control.Arrow.Serialize
import           Control.Arrow.Stack
import           Control.Arrow.State
import           Control.Arrow.StaticGlobalState
import           Control.Arrow.Table
import           Control.Arrow.Trans
import           Control.Arrow.Transformer.State
import           Control.Arrow.WasmFrame
import           Control.Arrow.Writer

import           Data.Profunctor


-- | Arrow transformer that adds a stack to a computation.
newtype StackT v c x y = StackT (StateT (JoinList v) c x y)
  deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowTrans,ArrowLift,ArrowRun,
            ArrowConst r, ArrowReader r, ArrowFail e, ArrowExcept e, ArrowWriter w,
            ArrowStaticGlobalState v1, ArrowFrame fd v1, ArrowMemory addr bytes,
            ArrowMemAddress v1 n addr, ArrowSerialize v1 bytes valTy loadTy storeTy,
            ArrowMemSizable v1, ArrowTable v1)

deriving instance (ArrowComplete (JoinList v, y) c) => ArrowComplete y (StackT v c)

---- | Execute a computation and only return the result value and store.
--runStackT :: StackT v c x y -> c (JoinList v, x) (JoinList v, y)
--runStackT = coerce
--{-# INLINE runStackT #-}
--
---- | Execute a computation and only return the result value.
--evalStackT :: (Profunctor c) => StackT v c x y -> c (JoinList v, x) y
--evalStackT f = rmap pi2 (runStackT f)
--
---- | Execute a computation and only return the result store.
--execStackT :: (Profunctor c) => StackT v c x y -> c (JoinList v, x) [v]
--execStackT f = rmap pi1 (runStackT f)

instance (ArrowChoice c, Profunctor c) => ArrowStack v (StackT v c) where
  push = StackT $ modify $ arr $ \(v, JoinList st) -> ((), JoinList $ v : st)
  pop = StackT $ modify $ arr $ \((), JoinList (v:st)) -> (v, JoinList st)
  peek = StackT $ get >>^ (\(JoinList vs) -> head vs)
  ifEmpty (StackT f) (StackT g) = StackT $ proc x -> do
    st <- get -< ()
    case st of
      [] -> g -< x
      _ -> f -< x
  localFreshStack (StackT f) = StackT $ proc x -> do
    (JoinList st) <- get -< ()
    put -< []
    y <- f -< x
    (JoinList stNew) <- get -< ()
    put -< JoinList $ stNew ++ st
    returnA -< y
  --pop2 = StackT $ modify $ arr $ \((),v2:v1:st) -> ((v1,v2), st)
  --popn = StackT $ modify $ arr $ \(n,st) -> splitAt (fromIntegral n) st
  --pushn = StackT $ modify $ arr $ \(st',st) -> ((),st'++st)

instance ArrowFix (Underlying (StackT v c) x y) => ArrowFix (StackT v c x y) where
    type Fix (StackT v c x y) = Fix (Underlying (StackT v c) x y)--StackT v (Fix c ([v],x) ([v],y))
