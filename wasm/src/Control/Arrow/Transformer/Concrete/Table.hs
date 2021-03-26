{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Arrow.Transformer.Concrete.Table where

import           Concrete

import           Control.Arrow
import           Control.Arrow.Const
import           Control.Arrow.Except
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.GlobalState
import           Control.Arrow.Logger
import           Control.Arrow.MemAddress
import           Control.Arrow.Memory
import           Control.Arrow.MemSizable
import           Control.Arrow.Reader
import           Control.Arrow.Serialize
import           Control.Arrow.Stack
import           Control.Arrow.State
import           Control.Arrow.StaticGlobalState
import           Control.Arrow.Store
import           Control.Arrow.Table
import           Control.Arrow.Trans
import           Control.Arrow.WasmFrame

import           Control.Arrow.Transformer.State

import           Control.Category

import           Data.Profunctor

newtype TableT c x y = TableT (StateT Tables c x y)
    deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowLift,
              ArrowFail e, ArrowExcept e, ArrowConst r, ArrowStore var' val', ArrowRun, ArrowFrame fd val,
              ArrowStack st, ArrowReader r, ArrowStaticGlobalState val,
              ArrowSerialize val dat valTy datDecTy datEncTy)

instance ArrowTable Value (TableT c) where
    -- TODO

instance ArrowTrans TableT where
    -- lift' :: c x y -> MemoryT v c x y
    lift' a = TableT (lift' a)

instance (ArrowLift c, ArrowFix (Underlying (TableT c) x y)) => ArrowFix (TableT c x y) where
    type Fix (TableT c x y) = Fix (Underlying (TableT c) x y)
