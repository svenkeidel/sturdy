{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Arrow.Transformer.Concrete.Memory where

import           Concrete

import           Control.Arrow
import           Control.Arrow.Const
import           Control.Arrow.Except
import           Control.Arrow.Fail
import           Control.Arrow.Fix
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
import           Data.Vector (Vector,(!),(//))
import qualified Data.Vector as Vec
import           Data.Word

import           Numeric.Natural (Natural)

import qualified Language.Wasm.Interpreter as Wasm

newtype MemoryT c x y = MemoryT (StateT Memories c x y)
    deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowLift,
              ArrowFail e, ArrowExcept e, ArrowConst r, ArrowStore var' val', ArrowRun, ArrowFrame fd val,
              ArrowStack st, ArrowReader r, ArrowStaticGlobalState val,
              ArrowSerialize val dat valTy datDecTy datEncTy, ArrowTable v)

instance ArrowTrans MemoryT where
  -- lift' :: c x y -> MemoryT v c x y
  lift' a = MemoryT (lift' a)

instance (ArrowChoice c, Profunctor c) => ArrowMemory Word32 (Vector Word8) (MemoryT c) where
  type Join y (MemoryT c) = ()
  memread (MemoryT sCont) (MemoryT eCont) = MemoryT $ proc (index,addr,size,x) -> do
      let addrI = fromIntegral addr
      mems <- get -< ()
      let (MemInst _ vec) = mems ! index
      case (addrI+size <= length vec) of
          True -> do
              let content = Vec.slice addrI size vec
              sCont -< (content,x)
          False -> do
              eCont -< x

  memstore (MemoryT sCont) (MemoryT eCont) = MemoryT $ proc (index,addr, content, x) -> do
      let addrI = fromIntegral addr
      mems <- get -< ()
      let (MemInst maxSize vec) = mems ! index
      let size = length content
      case (addrI+size <= length vec) of
          True -> do
              let ind = Vec.enumFromN addrI size
              let newMem = MemInst maxSize (Vec.update_ vec ind content)
              put -< mems // [(index,newMem)]
              sCont -< x
          False -> do
              eCont -< x

instance ArrowMemSizable Value (MemoryT c) where
  memsize = error "TODO: implement MemoryT.memsize"
  memgrow = error "TODO: implement MemoryT.memgrow"

instance (Arrow c, Profunctor c) => ArrowMemAddress Value Natural Word32 (MemoryT c) where
  memaddr = proc (Value (Wasm.VI32 base), off) -> returnA -< (base + fromIntegral off)

instance (ArrowLift c, ArrowFix (Underlying (MemoryT c) x y)) => ArrowFix (MemoryT c x y) where
  type Fix (MemoryT c x y) = Fix (Underlying (MemoryT c) x y)
