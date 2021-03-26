{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module UnitAnalysisValue where

import           Abstract
import           Data
import           GenericInterpreter

import           Control.Arrow
import           Control.Arrow.Order

import           Control.Arrow.Transformer.Value

import           Language.Wasm.Structure (BitSize(..), IUnOp(..), IBinOp(..), IRelOp(..), FUnOp(..),
                                          FBinOp(..), FRelOp(..), ValueType(..))

import           Data.Hashable
import           Data.Order
import           Data.Text.Prettyprint.Doc as Pretty

newtype Value = Value (BaseValue () () () ()) deriving (Eq, Show, Hashable, PreOrd, Complete, Pretty)

valueI32 = Value $ VI32 ()
valueI64 = Value $ VI64 ()
valueF32 = Value $ VF32 ()
valueF64 = Value $ VF64 ()

instance (ArrowChoice c) => IsVal Value (ValueT Value c) where
    type JoinVal y (ValueT Value c) = ArrowComplete y (ValueT Value c)

    i32const = proc _ -> returnA -< valueI32
    i64const = proc _ -> returnA -< valueI64
    f32const = proc _ -> returnA -< valueF32
    f64const = proc _ -> returnA -< valueF64
    iBinOp = proc (bs, op, Value v1, Value v2) ->
        case (bs,op,v1,v2) of
            (BS32, IAdd, VI32 _, VI32 _) -> returnA -< Just valueI32
            (BS32, IMul, VI32 _, VI32 _) -> returnA -< Just valueI32
            (BS32, ISub, VI32 _, VI32 _) -> returnA -< Just valueI32
            (BS64, IAdd, VI64 _, VI64 _) -> returnA -< Just valueI64
            (BS64, IMul, VI64 _, VI64 _) -> returnA -< Just valueI64
            (BS64, ISub, VI64 _, VI64 _) -> returnA -< Just valueI64
    iRelOp = proc (bs,op,Value v1, Value v2) ->
        case (bs,op,v1,v2) of
            (BS32, IEq, VI32 _, VI32 _) -> returnA -< valueI32
            (BS64, IEq, VI64 _, VI64 _) -> returnA -< valueI32
    i32ifNeqz f g = proc (Value v, x) -> do
        case v of
            (VI32 _) -> (f -< x) <⊔> (g -< x)
    ifHasType f g = proc (Value v,t,x) -> do
        case (v,t) of
            (VI32 _, I32) -> f -< x
            (VI64 _, I64) -> f -< x
            (VF32 _, F32) -> f -< x
            (VF64 _, F64) -> f -< x
            _                     -> g -< x

deriving instance ArrowComplete () c => ArrowComplete () (ValueT v c)
