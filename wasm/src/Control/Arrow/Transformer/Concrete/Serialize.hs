{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Arrow.Transformer.Concrete.Serialize where

import           Concrete
import           Data

import           Control.Arrow
import           Control.Arrow.Const
import           Control.Arrow.Except
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.Functions
import           Control.Arrow.Reader
import           Control.Arrow.Serialize
import           Control.Arrow.Stack
import           Control.Arrow.State
import           Control.Arrow.Globals
import           Control.Arrow.Store
import           Control.Arrow.Table
import           Control.Arrow.Trans
import           Control.Arrow.WasmFrame

import           Control.Category

import           Data.Bits (Bits, (.&.), shiftR, shiftL)
import           Data.Profunctor
import           Data.Vector (Vector)
import qualified Data.Vector as Vec
import           Data.Word

import           Language.Wasm.FloatUtils
import qualified Language.Wasm.Interpreter as Wasm
import           Language.Wasm.Structure hiding (exports, Const)

newtype SerializeT c x y = SerializeT (c x y)
    deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowLift,
              ArrowFail e, ArrowExcept e, ArrowConst r, ArrowStore var' val', ArrowRun, ArrowFrame fd val,
              ArrowStack st, ArrowState s, ArrowGlobals v, ArrowFunctions, ArrowReader m, ArrowTable v)

instance (Profunctor c, ArrowChoice c) => ArrowSerialize Value (Vector Word8) ValueType LoadType StoreType (SerializeT c) where
    decode sCont = proc (dat, decTy, valTy, x) -> do
      case (valTy,decTy) of
        (I32,L_I32) -> sCont -< (toVal Wasm.VI32 dat, x)
        (I64,L_I64) -> sCont -< (toVal Wasm.VI64 dat, x)
        (F32, L_F32) -> do
            let i = toIntegral dat
            sCont -< (Value $ Wasm.VF32 $ wordToFloat i, x)
        (F64, L_F64) -> do
            let i = toIntegral dat
            sCont -< (Value $ Wasm.VF64 $ wordToDouble i, x)
        (I32, L_I8S) -> do
            let i = toIntegral dat :: Word8
            let signedI = Wasm.asWord32 $ if i >= 128 then (-1) * fromIntegral (0xFF - i + 1) else fromIntegral i
            sCont -< (Value $ Wasm.VI32 signedI, x)
        (I32, L_I8U) -> sCont -< (toVal Wasm.VI32 dat, x)
        (I32, L_I16S) -> do
            let i = toIntegral dat :: Word16
            let signedI = Wasm.asWord32 $ if i >= 2^(15::Int) then (-1) * fromIntegral (0xFFFF - i + 1) else fromIntegral i
            sCont -< (Value $ Wasm.VI32 signedI, x)
        (I32, L_I16U) -> sCont -< (toVal Wasm.VI32 dat, x)
        (I64, L_I8S) -> do
            let i = toIntegral dat :: Word8
            let signedI = Wasm.asWord64 $ if i >= 128 then (-1) * fromIntegral (0xFF - i + 1) else fromIntegral i
            sCont -< (Value $ Wasm.VI64 signedI, x)
        (I64, L_I8U) -> sCont -< (toVal Wasm.VI64 dat, x)
        (I64, L_I16S) -> do
            let i = toIntegral dat :: Word16
            let signedI = Wasm.asWord64 $ if i >= 2^(15::Int) then (-1) * fromIntegral (0xFFFF - i + 1) else fromIntegral i
            sCont -< (Value $ Wasm.VI64 signedI, x)
        (I64, L_I16U) -> sCont -< (toVal Wasm.VI64 dat, x)
        (I64, L_I32S) -> do
            let i = toIntegral dat :: Word32
            let signedI = Wasm.asWord64 $ fromIntegral $ Wasm.asInt32 i
            sCont -< (Value $ Wasm.VI64 signedI, x)
        (I64, L_I32U) -> sCont -< (toVal Wasm.VI64 dat, x)
        _ -> returnA -< error "decode: do not support type"
        where
            toIntegral :: (Integral i, Bits i) => Vector Word8 -> i
            toIntegral bytes = Vec.foldr (\a b -> (b `shiftL` 8) + fromIntegral a) (fromIntegral (0::Int)) bytes
            toVal :: (Integral i, Bits i) => (i -> Wasm.Value) -> Vector Word8 -> Value
            toVal c bytes = Value $ c $ toIntegral bytes
    encode sCont = proc (Value val, valTy, datEncTy, x) -> do
      case (val, valTy, datEncTy) of
        (Wasm.VI32 v, I32, S_I32) -> do
            let vec = Vec.generate 4 (byte v)
            sCont -< (vec, x)
        (Wasm.VI64 v, I64, S_I64) -> do
            let vec = Vec.generate 8 (byte v)
            sCont -< (vec, x)
        (Wasm.VF32 v, F32, S_F32) -> do
            let vec = Vec.generate 4 (byte (floatToWord v))
            sCont -< (vec, x)
        (Wasm.VF64 v, F64, S_F64) -> do
            let vec = Vec.generate 8 (byte (doubleToWord v))
            sCont -< (vec, x)
        (Wasm.VI32 v, I32, S_I8) -> do
            let vec = Vec.generate 1 (byte v)
            sCont -< (vec, x)
        (Wasm.VI32 v, I32, S_I16) -> do
            let vec = Vec.generate 2 (byte v)
            sCont -< (vec, x)
        (Wasm.VI64 v, I64, S_I8) -> do
            let vec = Vec.generate 1 (byte v)
            sCont -< (vec, x)
        (Wasm.VI64 v, I64, S_I16) -> do
            let vec = Vec.generate 2 (byte v)
            sCont -< (vec, x)
        (Wasm.VI64 v, I64, S_I32) -> do
            let vec = Vec.generate 4 (byte v)
            sCont -< (vec, x)
        _ -> returnA -< error "encode: do not support type"

      where byte :: (Integral i, Bits i) => i -> Int -> Word8
            byte v i = fromIntegral $ (v `shiftR` (i*8)) .&. 0xFF

instance ArrowTrans SerializeT where
    lift' = SerializeT

instance (ArrowLift c, ArrowFix (Underlying (SerializeT c) x y)) => ArrowFix (SerializeT c x y) where
    type Fix (SerializeT c x y) = Fix (Underlying (SerializeT c) x y)
