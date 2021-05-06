{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Arrow.Transformer.Concrete.Memory where

import           Concrete
import           Data (pageSize)

import           Control.Arrow
import           Control.Arrow.Const
import           Control.Arrow.Except
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.Functions
import           Control.Arrow.EffectiveAddress
import           Control.Arrow.Memory
import           Control.Arrow.Reader
import           Control.Arrow.Serialize
import           Control.Arrow.Size
import           Control.Arrow.Stack
import           Control.Arrow.State
import           Control.Arrow.Globals
import           Control.Arrow.Store
import           Control.Arrow.Table
import           Control.Arrow.Trans
import           Control.Arrow.WasmFrame

import           Control.Arrow.Transformer.State

import           Control.Category

import           Data.Profunctor
import           Data.Vector (Vector,(!),(//))
import qualified Data.Vector as Vec
import           Data.Word

import           Numeric.Natural (Natural)

import qualified Language.Wasm.Interpreter as Wasm

newtype MemoryT c x y = MemoryT (StateT Memories c x y)
    deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowLift,
              ArrowFail e, ArrowExcept e, ArrowConst r, ArrowStore var' val', ArrowRun, ArrowFrame fd val,
              ArrowStack st, ArrowReader r, ArrowGlobals val, ArrowFunctions,
              ArrowSerialize val dat valTy datDecTy datEncTy, ArrowTable v)

instance ArrowTrans MemoryT where
  -- lift' :: c x y -> MemoryT v c x y
  lift' a = MemoryT (lift' a)

instance (ArrowChoice c, Profunctor c) => ArrowMemory Word32 (Vector Word8) Int (MemoryT c) where
    type Join y (MemoryT c) = ()
    memread (MemoryT sCont) (MemoryT eCont) = MemoryT $ proc (memIndex,addr,size,x) -> do
        let addrI = fromIntegral addr
        mems <- get -< ()
        let (MemInst _ vec) = mems ! memIndex
        if (addrI+size <= length vec)
          then do
            let bytes = Vec.slice addrI size vec
            sCont -< (bytes,x)
          else
            eCont -< x

    memstore (MemoryT sCont) (MemoryT eCont) = MemoryT $ proc (memIndex, addr, content, x) -> do
        let addrI = fromIntegral addr
        mems <- get -< ()
        let (MemInst maxSize vec) = mems ! memIndex
        let size = length content
        case (addrI+size <= length vec) of
            True -> do
                let ind = Vec.enumFromN addrI size
                let newMem = MemInst maxSize (Vec.update_ vec ind content)
                put -< mems // [(memIndex,newMem)]
                sCont -< x
            False -> do
                eCont -< x

    memsize = MemoryT $ proc memIndex -> do
        mems <- get -< ()
        let (MemInst _ vec) = mems ! memIndex
        let size = length vec
        returnA -< size `quot` pageSize
    memgrow (MemoryT _) (MemoryT eCont) = MemoryT $ proc (_,_,x) -> do
        -- TODO: allow to grow the memory
        eCont -< x

instance (ArrowChoice c, Profunctor c) => ArrowSize Value Int (MemoryT c) where
    valToSize = proc (Value v) -> case v of
        (Wasm.VI32 val) -> returnA -< fromIntegral val
        _ -> returnA -< error "valToSize: arguments needs to be an i32 integer"
    sizeToVal = proc sz -> returnA -< int32 $ fromIntegral sz

instance (Arrow c, Profunctor c) => ArrowEffectiveAddress Value Natural Word32 (MemoryT c) where
  effectiveAddress = proc (Value (Wasm.VI32 base), off) -> returnA -< (base + fromIntegral off)

instance (ArrowLift c, ArrowFix (Underlying (MemoryT c) x y)) => ArrowFix (MemoryT c x y) where
  type Fix (MemoryT c x y) = Fix (Underlying (MemoryT c) x y)
