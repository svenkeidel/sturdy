{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module UnitAnalysisValue where

import           Abstract
import           Data(joinList1'',Instruction)
import           GenericInterpreter hiding (Exc)
import qualified GenericInterpreter as Generic

import           Control.Arrow
import           Control.Arrow.Except
import           Control.Arrow.Fail as Fail
import           Control.Arrow.Order

import           Control.Arrow.Transformer.Value

import           Language.Wasm.Structure (BitSize(..), IBinOp(..), IRelOp(..), ValueType(..), IUnOp(..),
                                          FUnOp(..), FBinOp(..), FRelOp(..))
import qualified Language.Wasm.Interpreter as Wasm

import           Data.Hashable
import           Data.HashSet as HashSet
import           Data.Order
import           Data.Text.Prettyprint.Doc as Pretty

import           Text.Printf

newtype Exc v = Exc (HashSet (Generic.Exc v)) deriving (Eq, Show, Hashable, PreOrd, Complete)

instance (Show v) => Pretty (Exc v) where pretty = viaShow
instance (Show n) => Pretty (Instruction n) where pretty = viaShow


instance (ArrowExcept (Exc Value) c, ArrowChoice c) => IsException (Exc Value) Value (ValueT Value c) where
    type JoinExc y (ValueT Value c) = ArrowComplete y (ValueT Value c)
    exception = arr $ Exc . HashSet.singleton
    handleException f = proc (Exc excs,x) -> do
                            joinList1'' f -< (HashSet.toList excs,x)

newtype Value = Value (BaseValue () () () ()) deriving (Eq, Show, Hashable, PreOrd, Complete, Pretty)

valueI32, valueI64, valueF32, valueF64 :: Value
valueI32 = Value $ VI32 ()
valueI64 = Value $ VI64 ()
valueF32 = Value $ VF32 ()
valueF64 = Value $ VF64 ()

alpha :: Wasm.Value -> Value
alpha v = Value $ case v of
  Wasm.VI32 _ -> VI32 ()
  Wasm.VI64 _ -> VI64 ()
  Wasm.VF32 _ -> VF32 ()
  Wasm.VF64 _ -> VF64 ()


instance (ArrowChoice c, ArrowFail Err c, Fail.Join Value c) => IsVal Value (ValueT Value c) where
    type JoinVal y (ValueT Value c) = ArrowComplete y (ValueT Value c)

    i32const = proc _ -> returnA -< valueI32
    i64const = proc _ -> returnA -< valueI64
    f32const = proc _ -> returnA -< valueF32
    f64const = proc _ -> returnA -< valueF64

    iUnOp = proc (bs,op,Value v0) -> case (bs,op,v0) of
        (BS32, IClz,    VI32 _) -> returnA -< valueI32
        (BS32, ICtz,    VI32 _) -> returnA -< valueI32
        (BS32, IPopcnt, VI32 _) -> returnA -< valueI32
        (BS64, IClz,    VI64 _) -> returnA -< valueI64
        (BS64, ICtz,    VI64 _) -> returnA -< valueI64
        (BS64, IPopcnt, VI64 _) -> returnA -< valueI64
        _ -> returnA -< error "iUnOp: cannot apply operator to arguments"
    iBinOp = proc (bs,op,Value v1,Value v2) -> case (bs,op,v1,v2) of
        (BS32, IAdd,  VI32 _, VI32 _) -> returnA -< valueI32
        (BS32, ISub,  VI32 _, VI32 _) -> returnA -< valueI32
        (BS32, IMul,  VI32 _, VI32 _) -> returnA -< valueI32
        (BS32, IDivU, VI32 _, VI32 _) -> (returnA -< valueI32) <⊔> (trap -< "iBinOp: division by zero.")
        (BS32, IDivS, VI32 _, VI32 _) -> (returnA -< valueI32) <⊔> (trap -< "iBinOp: division by zero.")
        (BS32, IRemU, VI32 _, VI32 _) -> (returnA -< valueI32) <⊔> (trap -< "iBinOp: division by zero.")
        (BS32, IRemS, VI32 _, VI32 _) -> (returnA -< valueI32) <⊔> (trap -< "iBinOp: division by zero.")
        (BS32, IAnd,  VI32 _, VI32 _) -> returnA -< valueI32
        (BS32, IOr,   VI32 _, VI32 _) -> returnA -< valueI32
        (BS32, IXor,  VI32 _, VI32 _) -> returnA -< valueI32
        (BS32, IShl,  VI32 _, VI32 _) -> returnA -< valueI32
        (BS32, IShrU, VI32 _, VI32 _) -> returnA -< valueI32
        (BS32, IShrS, VI32 _, VI32 _) -> returnA -< valueI32
        (BS32, IRotl, VI32 _, VI32 _) -> returnA -< valueI32
        (BS32, IRotr, VI32 _, VI32 _) -> returnA -< valueI32

        (BS64, IAdd,  VI64 _, VI64 _) -> returnA -< valueI64
        (BS64, ISub,  VI64 _, VI64 _) -> returnA -< valueI64
        (BS64, IMul,  VI64 _, VI64 _) -> returnA -< valueI64
        (BS64, IDivU, VI64 _, VI64 _) -> (returnA -< valueI64) <⊔> (trap -< "iBinOp: division by zero.")
        (BS64, IDivS, VI64 _, VI64 _) -> (returnA -< valueI64) <⊔> (trap -< "iBinOp: division by zero.")
        (BS64, IRemU, VI64 _, VI64 _) -> (returnA -< valueI64) <⊔> (trap -< "iBinOp: division by zero.")
        (BS64, IRemS, VI64 _, VI64 _) -> (returnA -< valueI64) <⊔> (trap -< "iBinOp: division by zero.")
        (BS64, IAnd,  VI64 _, VI64 _) -> returnA -< valueI64
        (BS64, IOr,   VI64 _, VI64 _) -> returnA -< valueI64
        (BS64, IXor,  VI64 _, VI64 _) -> returnA -< valueI64
        (BS64, IShl,  VI64 _, VI64 _) -> returnA -< valueI64
        (BS64, IShrU, VI64 _, VI64 _) -> returnA -< valueI64
        (BS64, IShrS, VI64 _, VI64 _) -> returnA -< valueI64
        (BS64, IRotl, VI64 _, VI64 _) -> returnA -< valueI64
        (BS64, IRotr, VI64 _, VI64 _) -> returnA -< valueI64
        _ -> returnA -< error "iBinOp: cannot apply binary operator to given arguments."
    iRelOp = proc (bs,op,Value v1, Value v2) -> case (bs,op,v1,v2) of
        (BS32, IEq,  VI32 _, VI32 _) -> returnA -< valueI32
        (BS32, INe,  VI32 _, VI32 _) -> returnA -< valueI32
        (BS32, ILtU, VI32 _, VI32 _) -> returnA -< valueI32
        (BS32, ILtS, VI32 _, VI32 _) -> returnA -< valueI32
        (BS32, IGtU, VI32 _, VI32 _) -> returnA -< valueI32
        (BS32, IGtS, VI32 _, VI32 _) -> returnA -< valueI32
        (BS32, ILeU, VI32 _, VI32 _) -> returnA -< valueI32
        (BS32, ILeS, VI32 _, VI32 _) -> returnA -< valueI32
        (BS32, IGeU, VI32 _, VI32 _) -> returnA -< valueI32
        (BS32, IGeS, VI32 _, VI32 _) -> returnA -< valueI32

        (BS64, IEq,  VI64 _, VI64 _) -> returnA -< valueI32
        (BS64, INe,  VI64 _, VI64 _) -> returnA -< valueI32
        (BS64, ILtU, VI64 _, VI64 _) -> returnA -< valueI32
        (BS64, ILtS, VI64 _, VI64 _) -> returnA -< valueI32
        (BS64, IGtU, VI64 _, VI64 _) -> returnA -< valueI32
        (BS64, IGtS, VI64 _, VI64 _) -> returnA -< valueI32
        (BS64, ILeU, VI64 _, VI64 _) -> returnA -< valueI32
        (BS64, ILeS, VI64 _, VI64 _) -> returnA -< valueI32
        (BS64, IGeU, VI64 _, VI64 _) -> returnA -< valueI32
        (BS64, IGeS, VI64 _, VI64 _) -> returnA -< valueI32
        _ -> returnA -< error "iRelOp: cannot apply binary operator to given arguments."

    i32eqz = proc (Value v) -> case v of
        (VI32 _) -> returnA -< valueI32
        _ -> returnA -< error "i32eqz: cannot apply operator to given argument."
    i64eqz = proc (Value v) -> case v of
        (VI64 _) -> returnA -< valueI32
        _ -> returnA -< error "i64eqz: cannot apply operator to given argument."

    i32ifNeqz f g = proc (v, x) -> case v of
        Value (VI32 _) -> (g -< x) <⊔> (f -< x)
        _                   -> returnA -< error "i32ifNeqz: condition of unexpected type"

    ifHasType f g = proc (Value v,t,x) -> do
      case (v,t) of
        (VI32 _, I32) -> f -< x
        (VI64 _, I64) -> f -< x
        (VF32 _, F32) -> f -< x
        (VF64 _, F64) -> f -< x
        _             -> g -< x

    fUnOp = proc (bs,op,Value v) -> case (bs,op,v) of
        (BS32, FAbs,     VF32 _) -> returnA -< valueF32
        (BS32, FNeg,     VF32 _) -> returnA -< valueF32
        (BS32, FCeil,    VF32 _) -> returnA -< valueF32
        (BS32, FFloor,   VF32 _) -> returnA -< valueF32
        (BS32, FTrunc,   VF32 _) -> returnA -< valueF32
        (BS32, FNearest, VF32 _) -> returnA -< valueF32
        (BS32, FSqrt,    VF32 _) -> returnA -< valueF32
        (BS64, FAbs,     VF64 _) -> returnA -< valueF64
        (BS64, FNeg,     VF64 _) -> returnA -< valueF64
        (BS64, FCeil,    VF64 _) -> returnA -< valueF64
        (BS64, FFloor,   VF64 _) -> returnA -< valueF64
        (BS64, FTrunc,   VF64 _) -> returnA -< valueF64
        (BS64, FNearest, VF64 _) -> returnA -< valueF64
        (BS64, FSqrt,    VF64 _) -> returnA -< valueF64
        _ -> returnA -< error "fUnOp: cannot apply operator to arguements"
    fBinOp = proc (bs,op,Value v1,Value v2) -> case (bs,op,v1,v2) of
        (BS32, FAdd,      VF32 _, VF32 _) -> returnA -< valueF32
        (BS32, FSub,      VF32 _, VF32 _) -> returnA -< valueF32
        (BS32, FMul,      VF32 _, VF32 _) -> returnA -< valueF32
        (BS32, FDiv,      VF32 _, VF32 _) -> returnA -< valueF32
        (BS32, FMin,      VF32 _, VF32 _) -> returnA -< valueF32
        (BS32, FMax,      VF32 _, VF32 _) -> returnA -< valueF32
        (BS32, FCopySign, VF32 _, VF32 _) -> returnA -< valueF32
        (BS64, FAdd,      VF64 _, VF64 _) -> returnA -< valueF64
        (BS64, FSub,      VF64 _, VF64 _) -> returnA -< valueF64
        (BS64, FMul,      VF64 _, VF64 _) -> returnA -< valueF64
        (BS64, FDiv,      VF64 _, VF64 _) -> returnA -< valueF64
        (BS64, FMin,      VF64 _, VF64 _) -> returnA -< valueF64
        (BS64, FMax,      VF64 _, VF64 _) -> returnA -< valueF64
        (BS64, FCopySign, VF64 _, VF64 _) -> returnA -< valueF64
        _ -> returnA -< error "fBinOp: cannot apply binary operator to given arguments."
    fRelOp = proc (bs,op,Value v1,Value v2) -> case (bs,op,v1,v2) of
        (BS32, FEq, VF32 _, VF32 _) -> returnA -< valueI32
        (BS32, FNe, VF32 _, VF32 _) -> returnA -< valueI32
        (BS32, FLt, VF32 _, VF32 _) -> returnA -< valueI32
        (BS32, FGt, VF32 _, VF32 _) -> returnA -< valueI32
        (BS32, FLe, VF32 _, VF32 _) -> returnA -< valueI32
        (BS32, FGe, VF32 _, VF32 _) -> returnA -< valueI32
        (BS64, FEq, VF64 _, VF64 _) -> returnA -< valueI32
        (BS64, FNe, VF64 _, VF64 _) -> returnA -< valueI32
        (BS64, FLt, VF64 _, VF64 _) -> returnA -< valueI32
        (BS64, FGt, VF64 _, VF64 _) -> returnA -< valueI32
        (BS64, FLe, VF64 _, VF64 _) -> returnA -< valueI32
        (BS64, FGe, VF64 _, VF64 _) -> returnA -< valueI32
        _ -> returnA -< error "fRelOp: cannot apply binary operator to given arguments."
    i32WrapI64 = proc (Value v) -> case v of
        (VI64 _) -> returnA -< valueI32
        _ -> returnA -< error "i32WrapI64: cannot apply operator to given argument."
    iTruncFU = proc (bs1,bs2,x@(Value v)) -> do
      let errorTrunc = printf "iTruncFU: truncation operator from %s to %s failed on %s." (show bs1) (show bs2) (show x)
      case (bs1,bs2,v) of
        (BS32, BS32, VF32 _) -> (returnA -< valueI32) <⊔> (trap -< errorTrunc)
        (BS32, BS64, VF64 _) -> (returnA -< valueI32) <⊔> (trap -< errorTrunc)
        (BS64, BS32, VF32 _) -> (returnA -< valueI64) <⊔> (trap -< errorTrunc)
        (BS64, BS64, VF64 _) -> (returnA -< valueI64) <⊔> (trap -< errorTrunc)
        _ -> returnA -< error "iTruncFU: cannot apply operator to given argument."
    iTruncFS = proc (bs1,bs2,x@(Value v)) -> do
      let errorTrunc = printf "iTruncFS: truncation operator from %s to %s failed on %s." (show bs1) (show bs2) (show x)
      case (bs1,bs2,v) of
        (BS32, BS32, VF32 _) -> (returnA -< valueI32) <⊔> (trap -< errorTrunc)
        (BS32, BS64, VF64 _) -> (returnA -< valueI32) <⊔> (trap -< errorTrunc)
        (BS64, BS32, VF32 _) -> (returnA -< valueI64) <⊔> (trap -< errorTrunc)
        (BS64, BS64, VF64 _) -> (returnA -< valueI64) <⊔> (trap -< errorTrunc)
        _ -> returnA -< error "iTruncFS: cannot apply operator to given argument."
    i64ExtendSI32 = proc (Value v) -> case v of
        (VI32 _) -> returnA -< valueI64
        _ -> returnA -< error "i64ExtendSI32: cannot apply operator to given argument."
    i64ExtendUI32 = proc (Value v) -> case v of
        (VI32 _) -> returnA -< valueI64
        _ -> returnA -< error "i64ExtendUI32: cannot apply operator to given argument."
    fConvertIU = proc (bs1,bs2,Value v) -> case (bs1,bs2,v) of
        (BS32, BS32, VI32 _) -> returnA -< valueF32
        (BS32, BS64, VI64 _) -> returnA -< valueF32
        (BS64, BS32, VI32 _) -> returnA -< valueF64
        (BS64, BS64, VI64 _) -> returnA -< valueF64
        _ -> returnA -< error "fConvertIU: cannot apply operator to given argument."
    fConvertIS = proc (bs1,bs2,Value v) -> case (bs1,bs2,v) of
        (BS32, BS32, VI32 _) -> returnA -< valueF32
        (BS32, BS64, VI64 _) -> returnA -< valueF32
        (BS64, BS32, VI32 _) -> returnA -< valueF64
        (BS64, BS64, VI64 _) -> returnA -< valueF64
        _ -> returnA -< error "fConvertIS: cannot apply operator to given argument."
    f32DemoteF64 = proc (Value v) -> case v of
        (VF64 _) -> returnA -< valueF32
        _ -> returnA -< error "f32DemoteF64: cannot apply operator to given argument."
    f64PromoteF32 = proc (Value v) -> case v of
        (VF32 _) -> returnA -< valueF64
        _ -> returnA -< error "f64PromoteF32: cannot apply operator to given argument."
    iReinterpretF = proc (bs,Value v) -> case (bs,v) of
        (BS32, VF32 _) -> returnA -< valueI32
        (BS64, VF64 _) -> returnA -< valueI64
        _ -> returnA -< error "iReinterpretF: cannot apply operator to given argument."
    fReinterpretI = proc (bs,Value v) -> case (bs,v) of
        (BS32, VI32 _) -> returnA -< valueF32
        (BS64, VI64 _) -> returnA -< valueF64
        _ -> returnA -< error "fReinterpretI: cannot apply operator to given argument."
    listLookup sCont eCont = proc (Value v,xs,x) -> case v of
        (VI32 _) -> do
            (joinList1'' (proc (x,()) -> sCont -< x) -< (xs,())) <⊔> (eCont -< x)
        _ -> returnA -< error "listLookup: cannot apply operator to given arguments."

-- deriving instance ArrowComplete () c => ArrowComplete () (ValueT v c)
-- deriving instance ArrowComplete y c => ArrowComplete y (ValueT Value c)
