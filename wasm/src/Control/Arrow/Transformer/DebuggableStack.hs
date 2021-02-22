{-# LANGUAGE Arrows #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Transformer.DebuggableStack where

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Const
import           Control.Arrow.DebuggableStack
import           Control.Arrow.Except
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.Frame
import           Control.Arrow.Logger
import           Control.Arrow.MemAddress
import           Control.Arrow.Memory
import           Control.Arrow.MemSizable
import           Control.Arrow.Reader
import           Control.Arrow.Stack
import           Control.Arrow.State
import           Control.Arrow.Trans
import           Control.Arrow.Serialize
import           Control.Arrow.Transformer.Stack
import           Control.Arrow.GlobalState
import           Control.Arrow.Writer

import           Data.Profunctor

-- | Arrow transformer that adds a stack to a computation.
newtype DebuggableStackT v c x y = DebuggableStackT (StackT v c x y)
  deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowTrans,ArrowLift,ArrowRun,
            ArrowConst r, ArrowReader r, ArrowFail e, ArrowExcept e, ArrowStack v,
            ArrowGlobalState v1 , ArrowFrame fd v1, ArrowMemory addr bytes,
            ArrowMemAddress v1 n addr, ArrowSerialize v1 bytes valTy loadTy storeTy,
            ArrowMemSizable v1, ArrowWriter w, ArrowLogger l)

---- | Execute a computation and only return the result value and store.
--runStackT :: StackT v c x y -> c ([v], x) ([v], y)
--runStackT = coerce
--{-# INLINE runStackT #-}
--
---- | Execute a computation and only return the result value.
--evalStackT :: (Profunctor c) => StackT v c x y -> c ([v], x) y
--evalStackT f = rmap pi2 (runStackT f)
--
---- | Execute a computation and only return the result store.
--execStackT :: (Profunctor c) => StackT v c x y -> c ([v], x) [v]
--execStackT f = rmap pi1 (runStackT f)

instance (ArrowChoice c, Profunctor c) => ArrowDebuggableStack v (DebuggableStackT v c) where
  getStack = DebuggableStackT $ StackT $ get

--instance (ArrowChoice c, Profunctor c) => ArrowStack v (DebuggableStackT v c) where
--    pop = DebuggableStackT pop
--    push = DebuggableStackT push
--    peek = DebuggableStackT peek

--  --pop2 = StackT $ modify $ arr $ \((),v2:v1:st) -> ((v1,v2), st)
--  --popn = StackT $ modify $ arr $ \(n,st) -> splitAt (fromIntegral n) st
--  --pushn = StackT $ modify $ arr $ \(st',st) -> ((),st'++st)
--
instance ArrowFix (Underlying (DebuggableStackT v c) x y) => ArrowFix (DebuggableStackT v c x y) where
    type Fix (DebuggableStackT v c x y) = Fix (Underlying (DebuggableStackT v c) x y)--StackT v (Fix c ([v],x) ([v],y))
