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

import qualified Language.Wasm.Interpreter as Wasm
import           Language.Wasm.Structure hiding (exports, Const)

newtype SerializeT c x y = SerializeT (c x y)
    deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowLift,
              ArrowFail e, ArrowExcept e, ArrowConst r, ArrowStore var' val', ArrowRun, ArrowFrame fd val,
              ArrowStack st, ArrowState s, ArrowGlobals v, ArrowFunctions, ArrowReader m, ArrowTable v)

instance (Profunctor c, ArrowChoice c) => ArrowSerialize Value (Vector Word8) ValueType LoadType StoreType (SerializeT c) where
  decode sCont = proc (dat, decTy, valTy, x) -> do
    case (valTy,decTy) of
      (I32,L_I32) -> do
        let val = Vec.foldr (\w8 w32 -> (w32 `shiftL` 4) + fromIntegral w8) (fromIntegral $ Vec.last dat) dat
        let result = Value $ Wasm.VI32 val
        sCont -< (result, x)
      _ -> returnA -< error "decode: do not support type"
  encode sCont = proc (Value val, valTy, datEncTy, x) -> do
    case (val, valTy, datEncTy) of
      (Wasm.VI32 v, I32, S_I32) -> do
        let vec = Vec.generate 4 (byte v)
        sCont -< (vec, x)
      _ -> returnA -< error "encode: do not support type"

    where byte :: (Integral i, Bits i) => i -> Int -> Word8
          byte v i = fromIntegral $ (v `shiftR` (i*8)) .&. 0xFF

instance ArrowTrans SerializeT where
    lift' = SerializeT

instance (ArrowLift c, ArrowFix (Underlying (SerializeT c) x y)) => ArrowFix (SerializeT c x y) where
    type Fix (SerializeT c x y) = Fix (Underlying (SerializeT c) x y)
