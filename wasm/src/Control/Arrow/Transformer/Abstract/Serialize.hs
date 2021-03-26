{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Arrow.Transformer.Abstract.Serialize where

import           Abstract
import           Data
import           UnitAnalysisValue

import           Control.Arrow
import           Control.Arrow.Const
import           Control.Arrow.Except
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.Logger
import           Control.Arrow.Memory
import           Control.Arrow.Order
import           Control.Arrow.Reader
import           Control.Arrow.Serialize
import           Control.Arrow.Stack
import           Control.Arrow.State
import           Control.Arrow.StaticGlobalState
import           Control.Arrow.Store
import           Control.Arrow.Table
import           Control.Arrow.Trans
import           Control.Arrow.WasmFrame

import           Control.Category

import           Data.Order
import           Data.Profunctor

import           Language.Wasm.Structure (ValueType(..))

newtype SerializeT c x y = SerializeT (c x y)
    deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowLift,
              ArrowFail e, ArrowExcept e, ArrowConst r, ArrowStore var' val', ArrowRun, ArrowFrame fd val,
              ArrowStack st, ArrowLogger l, ArrowState s, ArrowStaticGlobalState v, ArrowReader m, ArrowTable v,
              ArrowMemory addr bytes, ArrowJoin)

instance ArrowTrans SerializeT where
    lift' = SerializeT

instance (Profunctor c, Arrow c) => ArrowSerialize Value () ValueType LoadType StoreType (SerializeT c) where
    decode sCont = proc ((),_,valTy,x) -> sCont -< (toTopVal valTy, x)
        where
            toTopVal I32 = Value $ VI32 top
            toTopVal I64 = Value $ VI64 top
            toTopVal F32 = Value $ VF32 top
            toTopVal F64 = Value $ VF64 top
    encode sCont = proc (_,_,_,x) -> sCont -< ((),x)

deriving instance (Arrow c, Profunctor c, ArrowComplete y c) => ArrowComplete y (SerializeT c)

instance (ArrowLift c, ArrowFix (Underlying (SerializeT c) x y)) => ArrowFix (SerializeT c x y) where
    type Fix (SerializeT c x y) = Fix (Underlying (SerializeT c) x y)
