{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module UnitAnalysisValue where

import           Abstract
import           Data()
import           GenericInterpreter

import           Control.Arrow
import           Control.Arrow.Order

import           Control.Arrow.Transformer.Value

import           Language.Wasm.Structure (BitSize(..), IBinOp(..), IRelOp(..), ValueType(..))

import           Data.Hashable
import           Data.Order
import           Data.Text.Prettyprint.Doc as Pretty

newtype Value = Value (BaseValue () () () ()) deriving (Eq, Show, Hashable, PreOrd, Complete, Pretty)

valueI32, valueI64, valueF32, valueF64 :: Value
valueI32 = Value $ VI32 ()
valueI64 = Value $ VI64 ()
valueF32 = Value $ VF32 ()
valueF64 = Value $ VF64 ()

instance ArrowChoice c => IsVal Value (ValueT Value c) where
    type JoinVal y (ValueT Value c) = ArrowComplete y (ValueT Value c)

    i32const = proc _ -> returnA -< valueI32
    i64const = proc _ -> returnA -< valueI64
    f32const = proc _ -> returnA -< valueF32
    f64const = proc _ -> returnA -< valueF64
    iBinOp _eCont = proc (bs, op, Value v1, Value v2) ->
      case (bs,op,v1,v2) of
        (BS32, IAdd, VI32 _, VI32 _) -> returnA -< valueI32
        (BS32, IMul, VI32 _, VI32 _) -> returnA -< valueI32
        (BS32, ISub, VI32 _, VI32 _) -> returnA -< valueI32
        (BS64, IAdd, VI64 _, VI64 _) -> returnA -< valueI64
        (BS64, IMul, VI64 _, VI64 _) -> returnA -< valueI64
        (BS64, ISub, VI64 _, VI64 _) -> returnA -< valueI64
        _ -> returnA -< error "iBinOp: cannot apply binary operator to given arguments."
    iRelOp = proc (bs,op,Value v1, Value v2) ->
      case (bs,op,v1,v2) of
        (BS32, IEq, VI32 _, VI32 _) -> returnA -< valueI32
        (BS64, IEq, VI64 _, VI64 _) -> returnA -< valueI32
        _ -> returnA -< error "iRelOp: cannot apply binary operator to given arguments."
    i32ifNeqz f g = proc (Value v, x) -> do
      case v of
        (VI32 _) -> (f -< x) <⊔> (g -< x)
        _ -> returnA -< error "i32ifNeqz: condition of unexpected type"
    ifHasType f g = proc (Value v,t,x) -> do
      case (v,t) of
        (VI32 _, I32) -> f -< x
        (VI64 _, I64) -> f -< x
        (VF32 _, F32) -> f -< x
        (VF64 _, F64) -> f -< x
        _             -> g -< x

    iUnOp = error "TODO: implement iUnOp"
    i32eqz = error "TODO: implement i32eqz"
    i64eqz = error "TODO: implement i64eqz"
    fUnOp = error "TODO: implement fUnOp"
    fBinOp = error "TODO: implement fBinOp"
    fRelOp = error "TODO: implement fRelOp"
    i32WrapI64 = error "TODO: implement i32WrapI64"
    iTruncFU = error "TODO: implement iTruncFU"
    iTruncFS = error "TODO: implement iTruncFS"
    i64ExtendSI32 = error "TODO: implement i64ExtendSI32"
    i64ExtendUI32 = error "TODO: implement i64ExtendUI32"
    fConvertIU = error "TODO: implement fConvertIU"
    fConvertIS = error "TODO: implement fConvertIS"
    f32DemoteF64 = error "TODO: implement f32DemoteF64"
    f64PromoteF32 = error "TODO: implement f64PromoteF32"
    iReinterpretF = error "TODO: implement iReinterpretF"
    fReinterpretI = error "TODO: implement IReinterpretI"
    listLookup = error "TODO: implement listLookup"


deriving instance ArrowComplete () c => ArrowComplete () (ValueT v c)
