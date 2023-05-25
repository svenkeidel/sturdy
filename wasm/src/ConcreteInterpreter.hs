{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module ConcreteInterpreter where

import           Data hiding (I32Eqz)
import           Concrete
import           GenericInterpreter hiding (eval,evalNumericInst,evalParametricInst,invokeExported,store)
import qualified GenericInterpreter as Generic

import           Control.Arrow
import qualified Control.Arrow.Trans as Trans
import           Control.Arrow.Except
import           Control.Arrow.Fail as Fail
import           Control.Arrow.Fix

import           Control.Arrow.Transformer.JumpTypes
import           Control.Arrow.Transformer.Stack
import           Control.Arrow.Transformer.StaticGlobalState
import           Control.Arrow.Transformer.Value
import           Control.Arrow.Transformer.WasmFrame

import           Control.Arrow.Transformer.Concrete.Failure
import           Control.Arrow.Transformer.Concrete.Except
import           Control.Arrow.Transformer.Concrete.Memory
import           Control.Arrow.Transformer.Concrete.Serialize
import           Control.Arrow.Transformer.Concrete.Table

import qualified Control.Monad.Primitive as Primitive

import           Data.Concrete.Error

import qualified Data.Function as Function
import           Data.IORef (writeIORef)
import qualified Data.Primitive.ByteArray as ByteArray
import           Data.Text.Lazy (Text)
import           Data.Text.Prettyprint.Doc (pretty, hsep)
import qualified Data.Vector as Vec
import           Data.Word
import           Data.Bits

import           Debug.Trace

import           Language.Wasm.FloatUtils
import           Language.Wasm.Interpreter (ModuleInstance, asInt32,asInt64,asWord32,asWord64,nearest,
                                            floatFloor, doubleFloor, floatCeil, doubleCeil,
                                            floatTrunc, doubleTrunc, zeroAwareMin, zeroAwareMax)
import qualified Language.Wasm.Interpreter as Wasm
import           Language.Wasm.Structure hiding (exports, Const, Instruction, Function,Expression,Memory,Table)
import           Language.Wasm.Validate (ValidModule)

import           Numeric.IEEE (copySign)
import           Text.Printf
--import           Prettyprinter (pretty, hsep)

--trap :: IsException (Exc v) v c => c String x
--trap = throw <<< exception <<^ Trap

instance (ArrowChoice c, ArrowFail Err c, Fail.Join Value c) => IsVal Value (ValueT Value c) where
    type JoinVal y (ValueT Value c) = ()

    i32const = proc w32 -> returnA -< int32 w32
    i64const = proc w64 -> returnA -< int64 w64
    f32const = proc f -> returnA -< float32 f
    f64const = proc d -> returnA -< float64 d

    iUnOp = proc (bs,op,Value v0) -> case (bs,op,v0) of
        (BS32, IClz,    Wasm.VI32 v) -> returnA -< int32 $ fromIntegral $ countLeadingZeros v
        (BS32, ICtz,    Wasm.VI32 v) -> returnA -< int32 $ fromIntegral $ countTrailingZeros v
        (BS32, IPopcnt, Wasm.VI32 v) -> returnA -< int32 $ fromIntegral $ popCount v
        (BS32, IExtend8S, Wasm.VI32 v) -> do
            let half = v .&. 0xFF
            returnA -< int32 $ if half >= 0x80 then asWord32 (fromIntegral half - 0x100) else half
        (BS32, IExtend16S, Wasm.VI32 v) -> do
            let half = v .&. 0xFFFF
            returnA -< int32 $ if half >= 0x8000 then asWord32 (fromIntegral half - 0x10000) else half
        (BS32, IExtend32S, Wasm.VI32 v) -> returnA -< int32 v
        (BS64, IClz,    Wasm.VI64 v) -> returnA -< int64 $ fromIntegral $ countLeadingZeros v
        (BS64, ICtz,    Wasm.VI64 v) -> returnA -< int64 $ fromIntegral $ countTrailingZeros v
        (BS64, IPopcnt, Wasm.VI64 v) -> returnA -< int64 $ fromIntegral $ popCount v
        (BS64, IExtend8S, Wasm.VI64 v) -> do
            let half = v .&. 0xFF
            returnA -< int64 $ if half >= 0x80 then asWord64 (fromIntegral half - 0x100) else half
        (BS64, IExtend16S, Wasm.VI64 v) -> do
            let quart = v .&. 0xFFFF
            returnA -< int64 $ if quart >= 0x8000 then asWord64 (fromIntegral quart - 0x10000) else quart
        (BS64, IExtend32S, Wasm.VI64 v) -> do
            let half = v .&. 0xFFFFFFFF
            returnA -< int64 $ if half >= 0x80000000 then asWord64 (fromIntegral half - 0x100000000) else half
        _ -> returnA -< error $ "iUnOp: cannot apply operator " ++ show op ++ " to arguement " ++ show v0 ++ " with " ++ show bs ++ "."
    iBinOp = proc (bs,op,Value v1,Value v2) -> case (bs,op,v1,v2) of
        (BS32, IAdd, Wasm.VI32 val1, Wasm.VI32 val2) -> returnA -< int32 $ val1 + val2
        (BS32, ISub, Wasm.VI32 val1, Wasm.VI32 val2) -> returnA -< int32 $ val1 - val2
        (BS32, IMul, Wasm.VI32 val1, Wasm.VI32 val2) -> returnA -< int32 $ val1 * val2
        (BS32, IDivU, Wasm.VI32 val1, Wasm.VI32 val2) ->
          if val2 == 0
          then trap -< "iBinOp: division by zero."
          else returnA -< int32 $ val1 `quot` val2
        (BS32, IDivS, Wasm.VI32 val1, Wasm.VI32 val2) ->
          if val2 == 0 || (val1 == 0x80000000 && val2 == 0xFFFFFFFF)
          then trap -< "iBinOp: division by zero."
          else returnA -< int32 $ asWord32 (asInt32 val1 `quot` asInt32 val2)
        (BS32, IRemU, Wasm.VI32 val1, Wasm.VI32 val2) ->
          if val2 == 0
          then trap -< "iBinOp: division by zero."
          else returnA -< int32 $ val1 `rem` val2
        (BS32, IRemS, Wasm.VI32 val1, Wasm.VI32 val2) ->
          if val2 == 0
          then trap -< "iBinOp: division by zero."
          else returnA -< int32 $ asWord32 (asInt32 val1 `rem` asInt32 val2)
        (BS32, IAnd,  Wasm.VI32 val1, Wasm.VI32 val2) -> returnA -< int32 $ val1 .&. val2
        (BS32, IOr,   Wasm.VI32 val1, Wasm.VI32 val2) -> returnA -< int32 $ val1 .|. val2
        (BS32, IXor,  Wasm.VI32 val1, Wasm.VI32 val2) -> returnA -< int32 $ val1 `xor` val2
        (BS32, IShl,  Wasm.VI32 val1, Wasm.VI32 val2) -> returnA -< int32 $ val1 `shiftL` fromIntegral (val2 `rem` 32)
        (BS32, IShrU, Wasm.VI32 val1, Wasm.VI32 val2) -> returnA -< int32 $ val1 `shiftR` fromIntegral (val2 `rem` 32)
        (BS32, IShrS, Wasm.VI32 val1, Wasm.VI32 val2) -> returnA -< int32 $ asWord32 $ asInt32 val1 `shiftR` fromIntegral (val2 `rem` 32)
        (BS32, IRotl, Wasm.VI32 val1, Wasm.VI32 val2) -> returnA -< int32 $ val1 `rotateL` fromIntegral val2
        (BS32, IRotr, Wasm.VI32 val1, Wasm.VI32 val2) -> returnA -< int32 $ val1 `rotateR` fromIntegral val2

        (BS64, IAdd,  Wasm.VI64 val1, Wasm.VI64 val2) -> returnA -< int64 $ val1 + val2
        (BS64, ISub,  Wasm.VI64 val1, Wasm.VI64 val2) -> returnA -< int64 $ val1 - val2
        (BS64, IMul,  Wasm.VI64 val1, Wasm.VI64 val2) -> returnA -< int64 $ val1 * val2
        (BS64, IDivU, Wasm.VI64 val1, Wasm.VI64 val2) ->
          if val2 == 0
          then trap -< "iBinOp: division by zero."
          else returnA -< int64 $ val1 `quot` val2
        (BS64, IDivS, Wasm.VI64 val1, Wasm.VI64 val2) ->
          if val2 == 0 || (val1 == 0x8000000000000000 && val2 == 0xFFFFFFFFFFFFFFFF)
          then trap -< "iBinOp: division by zero."
          else returnA -< int64 $ asWord64 (asInt64 val1 `quot` asInt64 val2)
        (BS64, IRemU, Wasm.VI64 val1, Wasm.VI64 val2) ->
          if val2 == 0
          then trap -< "iBinOp: division by zero."
          else returnA -< int64 $ val1 `rem` val2
        (BS64, IRemS, Wasm.VI64 val1, Wasm.VI64 val2) ->
          if val2 == 0
          then trap -< "iBinOp: division by zero."
          else returnA -< int64 $ asWord64 (asInt64 val1 `rem` asInt64 val2)
        (BS64, IAnd,  Wasm.VI64 val1, Wasm.VI64 val2) -> returnA -< int64 $ val1 .&. val2
        (BS64, IOr,   Wasm.VI64 val1, Wasm.VI64 val2) -> returnA -< int64 $ val1 .|. val2
        (BS64, IXor,  Wasm.VI64 val1, Wasm.VI64 val2) -> returnA -< int64 $ val1 `xor` val2
        (BS64, IShl,  Wasm.VI64 val1, Wasm.VI64 val2) -> returnA -< int64 $ val1 `shiftL` fromIntegral (val2 `rem` 64)
        (BS64, IShrU, Wasm.VI64 val1, Wasm.VI64 val2) -> returnA -< int64 $ val1 `shiftR` fromIntegral (val2 `rem` 64)
        (BS64, IShrS, Wasm.VI64 val1, Wasm.VI64 val2) -> returnA -< int64 $ asWord64 $ asInt64 val1 `shiftR` fromIntegral (val2 `rem` 64)
        (BS64, IRotl, Wasm.VI64 val1, Wasm.VI64 val2) -> returnA -< int64 $ val1 `rotateL` fromIntegral val2
        (BS64, IRotr, Wasm.VI64 val1, Wasm.VI64 val2) -> returnA -< int64 $ val1 `rotateR` fromIntegral val2
        _ -> returnA -< error "iBinOp: cannot apply binary operator to given arguments."
    iRelOp = proc (bs,op,Value v1, Value v2) -> case (bs,op,v1,v2) of
        (BS32, IEq,  Wasm.VI32 val1, Wasm.VI32 val2) -> returnA -< int32 $ if val1 == val2 then 1 else 0
        (BS32, INe,  Wasm.VI32 val1, Wasm.VI32 val2) -> returnA -< int32 $ if val1 /= val2 then 1 else 0
        (BS32, ILtU, Wasm.VI32 val1, Wasm.VI32 val2) -> returnA -< int32 $ if val1 <  val2 then 1 else 0
        (BS32, ILtS, Wasm.VI32 val1, Wasm.VI32 val2) -> returnA -< int32 $ if asInt32 val1 < asInt32 val2 then 1 else 0
        (BS32, IGtU, Wasm.VI32 val1, Wasm.VI32 val2) -> returnA -< int32 $ if val1 >  val2 then 1 else 0
        (BS32, IGtS, Wasm.VI32 val1, Wasm.VI32 val2) -> returnA -< int32 $ if asInt32 val1 > asInt32 val2 then 1 else 0
        (BS32, ILeU, Wasm.VI32 val1, Wasm.VI32 val2) -> returnA -< int32 $ if val1 <= val2 then 1 else 0
        (BS32, ILeS, Wasm.VI32 val1, Wasm.VI32 val2) -> returnA -< int32 $ if asInt32 val1 <= asInt32 val2 then 1 else 0
        (BS32, IGeU, Wasm.VI32 val1, Wasm.VI32 val2) -> returnA -< int32 $ if val1 >= val2 then 1 else 0
        (BS32, IGeS, Wasm.VI32 val1, Wasm.VI32 val2) -> returnA -< int32 $ if asInt32 val1 >= asInt32 val2 then 1 else 0

        (BS64, IEq,  Wasm.VI64 val1, Wasm.VI64 val2) -> returnA -< int32 $ if val1 == val2 then 1 else 0
        (BS64, INe,  Wasm.VI64 val1, Wasm.VI64 val2) -> returnA -< int32 $ if val1 /= val2 then 1 else 0
        (BS64, ILtU, Wasm.VI64 val1, Wasm.VI64 val2) -> returnA -< int32 $ if val1 <  val2 then 1 else 0
        (BS64, ILtS, Wasm.VI64 val1, Wasm.VI64 val2) -> returnA -< int32 $ if asInt64 val1 < asInt64 val2 then 1 else 0
        (BS64, IGtU, Wasm.VI64 val1, Wasm.VI64 val2) -> returnA -< int32 $ if val1 >  val2 then 1 else 0
        (BS64, IGtS, Wasm.VI64 val1, Wasm.VI64 val2) -> returnA -< int32 $ if asInt64 val1 > asInt64 val2 then 1 else 0
        (BS64, ILeU, Wasm.VI64 val1, Wasm.VI64 val2) -> returnA -< int32 $ if val1 <= val2 then 1 else 0
        (BS64, ILeS, Wasm.VI64 val1, Wasm.VI64 val2) -> returnA -< int32 $ if asInt64 val1 <= asInt64 val2 then 1 else 0
        (BS64, IGeU, Wasm.VI64 val1, Wasm.VI64 val2) -> returnA -< int32 $ if val1 >= val2 then 1 else 0
        (BS64, IGeS, Wasm.VI64 val1, Wasm.VI64 val2) -> returnA -< int32 $ if asInt64 val1 >= asInt64 val2 then 1 else 0
        _ -> returnA -< error "iRelOp: cannot apply binary operator to given arguments."

    i32eqz = proc (Value v) -> case v of
        (Wasm.VI32 val) -> returnA -< int32 $ if val == 0 then 1 else 0
        _ -> returnA -< error "i32eqz: cannot apply operator to given argument."
    i64eqz = proc (Value v) -> case v of
        (Wasm.VI64 val) -> returnA -< int32 $ if val == 0 then 1 else 0
        _ -> returnA -< error "i64eqz: cannot apply operator to given argument."

    i32ifNeqz f g = proc (v, x) -> case v of
        Value (Wasm.VI32 0) -> g -< x
        Value (Wasm.VI32 _) -> f -< x
        _                   -> returnA -< error "i32ifNeqz: condition of unexpected type"

    ifHasType f g = proc (v,t,x) -> do
                case (v,t) of
                    (Value (Wasm.VI32 _), I32) -> f -< x
                    (Value (Wasm.VI64 _), I64) -> f -< x
                    (Value (Wasm.VF32 _), F32) -> f -< x
                    (Value (Wasm.VF64 _), F64) -> f -< x
                    _                          -> g -< x

    fUnOp = proc (bs,op,Value v) -> case (bs,op,v) of
        (BS32, FAbs,     Wasm.VF32 val) -> returnA -< float32 $ abs val
        (BS32, FNeg,     Wasm.VF32 val) -> returnA -< float32 $ negate val
        (BS32, FCeil,    Wasm.VF32 val) -> returnA -< float32 $ floatCeil val
        (BS32, FFloor,   Wasm.VF32 val) -> returnA -< float32 $ floatFloor val
        (BS32, FTrunc,   Wasm.VF32 val) -> returnA -< float32 $ floatTrunc val
        (BS32, FNearest, Wasm.VF32 val) -> returnA -< float32 $ nearest val
        (BS32, FSqrt,    Wasm.VF32 val) -> returnA -< float32 $ sqrt val
        (BS64, FAbs,     Wasm.VF64 val) -> returnA -< float64 $ abs val
        (BS64, FNeg,     Wasm.VF64 val) -> returnA -< float64 $ negate val
        (BS64, FCeil,    Wasm.VF64 val) -> returnA -< float64 $ doubleCeil val
        (BS64, FFloor,   Wasm.VF64 val) -> returnA -< float64 $ doubleFloor val
        (BS64, FTrunc,   Wasm.VF64 val) -> returnA -< float64 $ doubleTrunc val
        (BS64, FNearest, Wasm.VF64 val) -> returnA -< float64 $ nearest val
        (BS64, FSqrt,    Wasm.VF64 val) -> returnA -< float64 $ sqrt val
        _ -> returnA -< error "fUnOp: cannot apply operator to arguements"
    fBinOp = proc (bs,op,Value v1,Value v2) -> case (bs,op,v1,v2) of
        (BS32, FAdd,      Wasm.VF32 val1, Wasm.VF32 val2) -> returnA -< float32 $ val1 + val2
        (BS32, FSub,      Wasm.VF32 val1, Wasm.VF32 val2) -> returnA -< float32 $ val1 - val2
        (BS32, FMul,      Wasm.VF32 val1, Wasm.VF32 val2) -> returnA -< float32 $ val1 * val2
        (BS32, FDiv,      Wasm.VF32 val1, Wasm.VF32 val2) -> returnA -< float32 $ val1 / val2
        (BS32, FMin,      Wasm.VF32 val1, Wasm.VF32 val2) -> returnA -< float32 $ zeroAwareMin val1 val2
        (BS32, FMax,      Wasm.VF32 val1, Wasm.VF32 val2) -> returnA -< float32 $ zeroAwareMax val1 val2
        (BS32, FCopySign, Wasm.VF32 val1, Wasm.VF32 val2) -> returnA -< float32 $ copySign val1 val2
        (BS64, FAdd,      Wasm.VF64 val1, Wasm.VF64 val2) -> returnA -< float64 $ val1 + val2
        (BS64, FSub,      Wasm.VF64 val1, Wasm.VF64 val2) -> returnA -< float64 $ val1 - val2
        (BS64, FMul,      Wasm.VF64 val1, Wasm.VF64 val2) -> returnA -< float64 $ val1 * val2
        (BS64, FDiv,      Wasm.VF64 val1, Wasm.VF64 val2) -> returnA -< float64 $ val1 / val2
        (BS64, FMin,      Wasm.VF64 val1, Wasm.VF64 val2) -> returnA -< float64 $ zeroAwareMin val1 val2
        (BS64, FMax,      Wasm.VF64 val1, Wasm.VF64 val2) -> returnA -< float64 $ zeroAwareMax val1 val2
        (BS64, FCopySign, Wasm.VF64 val1, Wasm.VF64 val2) -> returnA -< float64 $ copySign val1 val2
        _ -> returnA -< error "fBinOp: cannot apply binary operator to given arguments."
    fRelOp = proc (bs,op,Value v1,Value v2) -> case (bs,op,v1,v2) of
        (BS32, FEq, Wasm.VF32 val1, Wasm.VF32 val2) -> returnA -< int32 $ if val1 == val2 then 1 else 0
        (BS32, FNe, Wasm.VF32 val1, Wasm.VF32 val2) -> returnA -< int32 $ if val1 /= val2 then 1 else 0
        (BS32, FLt, Wasm.VF32 val1, Wasm.VF32 val2) -> returnA -< int32 $ if val1 <  val2 then 1 else 0
        (BS32, FGt, Wasm.VF32 val1, Wasm.VF32 val2) -> returnA -< int32 $ if val1 >  val2 then 1 else 0
        (BS32, FLe, Wasm.VF32 val1, Wasm.VF32 val2) -> returnA -< int32 $ if val1 <= val2 then 1 else 0
        (BS32, FGe, Wasm.VF32 val1, Wasm.VF32 val2) -> returnA -< int32 $ if val1 >= val2 then 1 else 0
        (BS64, FEq, Wasm.VF64 val1, Wasm.VF64 val2) -> returnA -< int32 $ if val1 == val2 then 1 else 0
        (BS64, FNe, Wasm.VF64 val1, Wasm.VF64 val2) -> returnA -< int32 $ if val1 /= val2 then 1 else 0
        (BS64, FLt, Wasm.VF64 val1, Wasm.VF64 val2) -> returnA -< int32 $ if val1 <  val2 then 1 else 0
        (BS64, FGt, Wasm.VF64 val1, Wasm.VF64 val2) -> returnA -< int32 $ if val1 >  val2 then 1 else 0
        (BS64, FLe, Wasm.VF64 val1, Wasm.VF64 val2) -> returnA -< int32 $ if val1 <= val2 then 1 else 0
        (BS64, FGe, Wasm.VF64 val1, Wasm.VF64 val2) -> returnA -< int32 $ if val1 >= val2 then 1 else 0
        _ -> returnA -< error "fRelOp: cannot apply binary operator to given arguments."
    i32WrapI64 = proc (Value v) -> case v of
        (Wasm.VI64 val) -> returnA -< int32 $ fromIntegral $ val .&. 0xFFFFFFFF
        _ -> returnA -< error "i32WrapI64: cannot apply operator to given argument."
    iTruncFU = proc (bs1,bs2,x@(Value v)) -> do
      let errorTrunc = printf "iTruncFU: truncation operator from %s to %s failed on %s." (show bs1) (show bs2) (show x)
      case (bs1,bs2,v) of
        (BS32, BS32, Wasm.VF32 val) ->
            if isNaN val || isInfinite val || val >= 2^32 || val <= -1
            then trap -< errorTrunc
            else returnA -< int32 $ truncate val
        (BS32, BS64, Wasm.VF64 val) ->
            if isNaN val || isInfinite val || val >= 2^32 || val <= -1
            then trap -< errorTrunc
            else returnA -< int32 $ truncate val
        (BS64, BS32, Wasm.VF32 val) ->
            if isNaN val || isInfinite val || val >= 2^64 || val <= -1
            then trap -< errorTrunc
            else returnA -< int64 $ truncate val
        (BS64, BS64, Wasm.VF64 val) ->
            if isNaN val || isInfinite val || val >= 2^64 || val <= -1
            then trap -< errorTrunc
            else returnA -< int64 $ truncate val
        _ -> returnA -< error errorTrunc
    iTruncFS = proc (bs1,bs2,x@(Value v)) -> do
      let errorTrunc = printf "iTruncFS: truncation operator from %s to %s failed on %s." (show bs1) (show bs2) (show x)
      case (bs1,bs2,v) of
        (BS32, BS32, Wasm.VF32 val) ->
            if isNaN val || isInfinite val || val >= 2^31 || val < -2^31 - 1
            then trap -< errorTrunc
            else returnA -< int32 $ truncate val
        (BS32, BS64, Wasm.VF64 val) ->
            if isNaN val || isInfinite val || val >= 2^31 || val <= -2^31 - 1
            then trap -< errorTrunc
            else returnA -< int32 $ truncate val
        (BS64, BS32, Wasm.VF32 val) ->
            if isNaN val || isInfinite val || val >= 2^63 || val < -2^63 - 1
            then trap -< errorTrunc
            else returnA -< int64 $ truncate val
        (BS64, BS64, Wasm.VF64 val) ->
            if isNaN val || isInfinite val || val >= 2^63 || val < -2^63 - 1
            then trap -< errorTrunc
            else returnA -< int64 $ truncate val
        _ -> returnA -< error errorTrunc
    iTruncSatFU = proc (bs1,bs2,x@(Value v)) -> do
        let errorTrunc = printf "iTruncSatFU: truncation operator from %s to %s failed on %s." (show bs1) (show bs2) (show x)
        case (bs1,bs2,v) of
            (BS32, BS32, Wasm.VF32 val) ->
                if val <= -1 || isNaN val
                then returnA -< int32 0
                else if val >= 2^32
                     then returnA -< int32 0xffffffff
                     else returnA -< int32 $ truncate val
            (BS32, BS64, Wasm.VF64 val) ->
                if val <= -1 || isNaN val
                then returnA -< int32 0
                else if val >= 2^32
                     then returnA -< int32 0xffffffff
                     else returnA -< int32 $ truncate val
            (BS64, BS32, Wasm.VF32 val) ->
                if val <= -1 || isNaN val
                then returnA -< int64 0
                else if val >= 2^64
                     then returnA -< int64 0xffffffffffffffff
                     else returnA -< int64 $ truncate val
            (BS64, BS64, Wasm.VF64 val) ->
                if val <= -1 || isNaN val
                then returnA -< int64 0
                else if val >= 2^64
                     then returnA -< int64 0xffffffffffffffff
                     else returnA -< int64 $ truncate val
            _ -> returnA -< error errorTrunc
    iTruncSatFS = proc (bs1,bs2,x@(Value v)) -> do
        let errorTrunc = printf "iTruncSatFS: truncation operator from %s to %s failed on %s." (show bs1) (show bs2) (show x)
        case (bs1,bs2,v) of
            (BS32, BS32, Wasm.VF32 val) ->
                if isNaN val
                then returnA -< int32 0
                else if val >= 2^31
                     then returnA -< int32 0x7fffffff
                     else if val <= -2^31 - 1
                          then returnA -< int32 0x80000000
                          else returnA -< int32 $ asWord32 $ truncate val
            (BS32, BS64, Wasm.VF64 val) ->
                if isNaN val
                then returnA -< int32 0
                else if val >= 2^31
                     then returnA -< int32 0x7fffffff
                     else if val <= -2^31 - 1
                          then returnA -< int32 0x80000000
                          else returnA -< int32 $ asWord32 $ truncate val
            (BS64, BS32, Wasm.VF32 val) ->
                if isNaN val
                then returnA -< int64 0
                else if val >= 2^63
                     then returnA -< int64 0x7fffffffffffffff
                     else if val <= -2^63 - 1
                          then returnA -< int64 0x8000000000000000
                          else returnA -< int64 $ asWord64 $ truncate val
            (BS64, BS64, Wasm.VF64 val) ->
                if isNaN val
                then returnA -< int64 0
                else if val >= 2^63
                     then returnA -< int64 0x7fffffffffffffff
                     else if val <= -2^63 - 1
                          then returnA -< int64 0x8000000000000000
                          else returnA -< int64 $ asWord64 $ truncate val
            _ -> returnA -< error errorTrunc
    i64ExtendSI32 = proc (Value v) -> case v of
        (Wasm.VI32 val) -> returnA -< int64 $ asWord64 $ fromIntegral $ asInt32 val
        _ -> returnA -< error "i64ExtendSI32: cannot apply operator to given argument."
    i64ExtendUI32 = proc (Value v) -> case v of
        (Wasm.VI32 val) -> returnA -< int64 $ fromIntegral val
        _ -> returnA -< error "i64ExtendUI32: cannot apply operator to given argument."
    fConvertIU = proc (bs1,bs2,Value v) -> case (bs1,bs2,v) of
        (BS32, BS32, Wasm.VI32 val) -> returnA -< float32 $ realToFrac val
        (BS32, BS64, Wasm.VI64 val) -> returnA -< float32 $ realToFrac val
        (BS64, BS32, Wasm.VI32 val) -> returnA -< float64 $ realToFrac val
        (BS64, BS64, Wasm.VI64 val) -> returnA -< float64 $ realToFrac val
        _ -> returnA -< error "fConvertIU: cannot apply operator to given argument."
    fConvertIS = proc (bs1,bs2,Value v) -> case (bs1,bs2,v) of
        (BS32, BS32, Wasm.VI32 val) -> returnA -< float32 $ realToFrac $ asInt32 val
        (BS32, BS64, Wasm.VI64 val) -> returnA -< float32 $ realToFrac $ asInt64 val
        (BS64, BS32, Wasm.VI32 val) -> returnA -< float64 $ realToFrac $ asInt32 val
        (BS64, BS64, Wasm.VI64 val) -> returnA -< float64 $ realToFrac $ asInt64 val
        _ -> returnA -< error "fConvertIS: cannot apply operator to given argument."
    f32DemoteF64 = proc (Value v) -> case v of
        (Wasm.VF64 val) -> returnA -< float32 $ realToFrac val
        _ -> returnA -< error "f32DemoteF64: cannot apply operator to given argument."
    f64PromoteF32 = proc (Value v) -> case v of
        (Wasm.VF32 val) -> returnA -< float64 $ realToFrac val
        _ -> returnA -< error "f64PromoteF32: cannot apply operator to given argument."
    iReinterpretF = proc (bs,Value v) -> case (bs,v) of
        (BS32, Wasm.VF32 val) -> returnA -< int32 $ floatToWord val
        (BS64, Wasm.VF64 val) -> returnA -< int64 $ doubleToWord val
        _ -> returnA -< error "iReinterpretF: cannot apply operator to given argument."
    fReinterpretI = proc (bs,Value v) -> case (bs,v) of
        (BS32, Wasm.VI32 val) -> returnA -< float32 $ wordToFloat val
        (BS64, Wasm.VI64 val) -> returnA -< float64 $ wordToDouble val
        _ -> returnA -< error "fReinterpretI: cannot apply operator to given argument."
    listLookup sCont eCont = proc (Value v,xs,x) -> case v of
        (Wasm.VI32 val) -> if fromIntegral val < length xs
                           then sCont -< xs !! fromIntegral val
                           else eCont -< x
        _ -> returnA -< error "listLookup: cannot apply operator to given arguments."

instance (ArrowExcept (Exc Value) c) => IsException (Exc Value) Value (ValueT Value c) where
    type JoinExc y (ValueT Value c) = ()
    exception = arr id
    handleException = id

type Result = (Error
                             Err
                             (JoinVector Value,
                              (Tables,
                               (Memories,
                                (StaticGlobalState Value, Error (Exc Value) (JoinList Value, [Value]))))))


invokeExported :: StaticGlobalState Value
                        -> Memories
                        -> Tables
                        -> ModuleInstance
                        -> Text
                        -> [Value]
                        -> Result
invokeExported staticS mem tab modInst funcName args =
    let ?fixpointAlgorithm = Function.fix in
    --let ?fixpointAlgorithm = fixpointAlgorithm (trace p1 p2) in
    Trans.run
    (Generic.invokeExported ::
      ValueT Value
        (JumpTypesT
          (StackT Value
            (ExceptT (Generic.Exc Value)
              (StaticGlobalStateT Value
                (MemoryT
                  (SerializeT
                    (TableT
                      (FrameT FrameData Value
                        (FailureT Err
                          (->)))))))))) (Text, [Value]) [Value]) (JoinVector Vec.empty,((0,modInst),(tab,(mem,(staticS,([],([],(funcName,args))))))))
    where
        p1 (locals,(_,(_,(_,(_,(stack,(_,instr))))))) = hsep [pretty stack, pretty locals, pretty instr]
        p2 (Success (_, (_,(_,(_, Success (stack,_)))))) = pretty stack
        p2 _ = pretty "Fail"

invokeExported' :: Wasm.Store -> ModuleInstance -> Text -> [Wasm.Value] -> IO (Maybe [Wasm.Value])
invokeExported' store modInst funcName wasmArgs = do
    (gs, ms, ts) <- storeToGlobalState store Value toMem TableInst
    let res = invokeExported gs ms ts modInst funcName (map Value wasmArgs)
    case res of
        (Fail _) -> return Nothing
        (Success (_,(_,(newMems,(StaticGlobalState _ sturdyGlobs,Success (_,funcRes)))))) -> do
            let storeMems = Wasm.memInstances store
            let wasmGlobs = Wasm.globalInstances store
            Vec.zipWithM_ (\(MemInst _ vec) (Wasm.MemoryInstance _ oldMem) -> do
                            newMem <- toByteArray vec
                            writeIORef oldMem newMem) newMems storeMems
            Vec.zipWithM_ (\(GlobInst _ (Value v)) g ->
                case g of
                    Wasm.GIMut _ ref -> writeIORef ref v
                    _ -> return ()) sturdyGlobs wasmGlobs
            return $ Just (map (\(Value x) -> x) funcRes)
        _ -> error "cannot happen in valid module"
    where
        toMem size lst = MemInst size (Vec.fromList lst)
        toByteArray :: Vec.Vector Word8 -> IO (ByteArray.MutableByteArray (Primitive.PrimState IO))
        toByteArray vec = do
            mem  <- ByteArray.newByteArray (Vec.length vec)
            Vec.imapM_ (ByteArray.writeByteArray mem) vec
            return mem


instantiateConcrete :: ValidModule -> IO (Either String (ModuleInstance, StaticGlobalState Value, Memories, Tables))
instantiateConcrete valMod = instantiate valMod Value toMem TableInst
    where
        toMem size lst = MemInst size (Vec.fromList lst)
