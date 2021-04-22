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
import qualified TaintAnalysisValue as Taint
import           UnitAnalysisValue

import           Control.Arrow
import           Control.Arrow.Const
import           Control.Arrow.Except
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.Logger
import           Control.Arrow.MemAddress
import           Control.Arrow.Memory
import           Control.Arrow.Order
import           Control.Arrow.Reader
import           Control.Arrow.Serialize
import           Control.Arrow.Size
import           Control.Arrow.Stack
import           Control.Arrow.State
import           Control.Arrow.StaticGlobalState
import           Control.Arrow.Store
import           Control.Arrow.Table
import           Control.Arrow.Trans
import           Control.Arrow.WasmFrame

import           Control.Category

import           Data.Coerce (coerce)
import           Data.Order
import           Data.Profunctor

import           Language.Wasm.Structure (ValueType(..))

newtype SerializeT v c x y = SerializeT (c x y)
    deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowLift,
              ArrowFail e, ArrowExcept e, ArrowConst r, ArrowStore var' val', ArrowRun, ArrowFrame fd val,
              ArrowStack st, ArrowLogger l, ArrowState s, ArrowStaticGlobalState v, ArrowReader m, ArrowTable v,
              ArrowMemory addr bytes sz, ArrowSize v sz, ArrowJoin, ArrowMemAddress base off addr)

instance ArrowTrans (SerializeT v) where
    lift' = SerializeT

instance (Profunctor c, Arrow c) => ArrowSerialize Value Bytes ValueType LoadType StoreType (SerializeT Value c) where
    decode sCont = proc (Bytes,_,valTy,x) -> sCont -< (toTopVal valTy, x)
        where
            toTopVal I32 = Value $ VI32 top
            toTopVal I64 = Value $ VI64 top
            toTopVal F32 = Value $ VF32 top
            toTopVal F64 = Value $ VF64 top
    encode sCont = proc (_,_,_,x) -> sCont -< (Bytes,x)


tainted :: Arrow c => SerializeT v c x v -> SerializeT (Taint.Value v) c x (Taint.Value v)
tainted f = proc x -> do
  v <- liftSerializeT f -< x
  returnA -< Taint.Value Taint.Untainted v
{-# INLINE tainted #-}

liftSerializeT :: SerializeT v c x y -> SerializeT (Taint.Value v) c x y
liftSerializeT = coerce
{-# INLINE liftSerializeT #-}

unliftSerializeT :: SerializeT (Taint.Value v) c x y -> SerializeT v c x y
unliftSerializeT = coerce
{-# INLINE unliftSerializeT #-}

instance (Arrow c, ArrowSerialize v Bytes ValueType LoadType StoreType (SerializeT v c)) => ArrowSerialize (Taint.Value v) Bytes ValueType LoadType StoreType (SerializeT (Taint.Value v) c) where
    decode sCont = proc (bytes,datDecTy,valTy,x) ->
        liftSerializeT (decode
          (proc (v,x) -> (unliftSerializeT sCont) -< (Taint.Value Taint.Tainted v,x)))
          -< (bytes,datDecTy,valTy,x)
    encode sCont = proc (Taint.Value _t v,valTy,datEncTy,x) ->
        liftSerializeT (encode
          (unliftSerializeT sCont))
          -< (v,valTy,datEncTy,x)

deriving instance (Arrow c, Profunctor c, ArrowComplete y c) => ArrowComplete y (SerializeT v c)

instance (ArrowLift c, ArrowFix (Underlying (SerializeT v c) x y)) => ArrowFix (SerializeT v c x y) where
    type Fix (SerializeT v c x y) = Fix (Underlying (SerializeT v c) x y)
