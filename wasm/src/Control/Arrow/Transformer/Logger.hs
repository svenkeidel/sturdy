{-# LANGUAGE Arrows #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Arrow.Transformer.Logger where

--import           Control.Category
--import           Control.Arrow
--import           Control.Arrow.Const
--import           Control.Arrow.DebuggableStack
--import           Control.Arrow.Except
--import           Control.Arrow.Fail
--import           Control.Arrow.Fix
--import           Control.Arrow.Logger
--import           Control.Arrow.MemAddress
--import           Control.Arrow.Memory
--import           Control.Arrow.MemSizable
--import           Control.Arrow.Order
--import           Control.Arrow.Reader
--import           Control.Arrow.Stack
--import           Control.Arrow.Trans
--import           Control.Arrow.Serialize
--import           Control.Arrow.Transformer.Stack
--import           Control.Arrow.GlobalState
--import           Control.Arrow.Writer
--import           Control.Arrow.WasmFrame
--
--import           Data.Profunctor

--newtype LoggerT v c x y = LoggerT (StackT v c x y)
--    deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowTrans, ArrowLift, ArrowRun,
--              ArrowConst r, ArrowReader r, ArrowFail e, ArrowExcept e,
--              ArrowGlobalState v1 m, ArrowFrame fd v1, ArrowMemory addr bytes,
--              ArrowMemAddress v1 n addr, ArrowSerialize v1 bytes valTy loadTy storeTy,
--              ArrowMemSizable v1, ArrowWriter w)
--
--instance (Arrow c, Profunctor c, ArrowStack v c) => ArrowStack v (LoggerT l c) where
--    push = lift' push
--    pop = lift' pop
--    peek = lift' peek
--    ifEmpty a1 a2 = lift (ifEmpty (unlift a1) (unlift a2))
--    localFreshStack a = lift (localFreshStack (unlift a))
--
--instance(Arrow c, Profunctor c, ArrowDebuggableStack v c) => ArrowDebuggableStack v (LoggerT l c) where
--    getStack = lift' getStack
--
--instance (ArrowChoice c, Profunctor c) => ArrowLogger v (LoggerT v c) where
--    log = LoggerT push
--
--
--instance (Arrow c, Profunctor c, ArrowComplete ([v],y) c) => ArrowComplete y (LoggerT v c) where
--    f <⊔> g = lift $ unlift f <⊔> unlift g
--
--instance ArrowFix (Underlying (LoggerT v c) x y) => ArrowFix (LoggerT v c x y) where
--    type Fix (LoggerT v c x y) = Fix (Underlying (LoggerT v c) x y)--StackT v (Fix c ([v],x) ([v],y))
