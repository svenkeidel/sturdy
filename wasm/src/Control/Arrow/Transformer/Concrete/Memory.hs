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
import           Control.Arrow.GlobalState
import           Control.Arrow.Logger
import           Control.Arrow.Memory
import           Control.Arrow.Reader
import           Control.Arrow.Stack
import           Control.Arrow.State
import           Control.Arrow.Store
import           Control.Arrow.Trans
import           Control.Arrow.WasmFrame

import           Control.Category

import           Data.Profunctor
import           Data.Vector (Vector)
import qualified Data.Vector as Vec
import           Data.Word

newtype MemoryT c x y = MemoryT (c x y)
    deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowLift,
              ArrowFail e, ArrowExcept e, ArrowConst r, ArrowStore var' val', ArrowRun, ArrowFrame fd val,
              ArrowStack st, ArrowLogger l, ArrowState s, ArrowReader r, ArrowGlobalState val m)--, ArrowState (GlobalState v))

instance ArrowTrans MemoryT where
    -- lift' :: c x y -> MemoryT v c x y
    lift' = MemoryT

instance (ArrowChoice c, Profunctor c) => ArrowMemory MemInst Word32 (Vector Word8) (MemoryT c) where
    type Join y (MemoryT c) = ()
    memread (MemoryT sCont) (MemoryT eCont) = MemoryT $ proc (m@(MemInst _ vec), (addr, size, x)) -> do
        let addrI = fromIntegral addr
        case (addrI+size <= length vec) of
            True -> do
                let content = Vec.slice addrI size vec
                y <- sCont -< (content,x)
                returnA -< (m,y)
            False -> do
                y <- eCont -< x
                returnA -< (m,y)

    memstore (MemoryT sCont) (MemoryT eCont) = MemoryT $ proc (m@(MemInst maxSize vec), (addr, content, x)) -> do
        let addrI = fromIntegral addr
        let size = length content
        case (addrI+size <= length vec) of
            True -> do
                let ind = Vec.enumFromN addrI size
                let newMem = MemInst maxSize (Vec.update_ vec ind content)
                y <- sCont -< x
                returnA -< (newMem,y)
            False -> do
                y <- eCont -< x
                returnA -< (m,y)

instance (ArrowLift c, ArrowFix (Underlying (MemoryT c) x y)) => ArrowFix (MemoryT c x y) where
    type Fix (MemoryT c x y) = Fix (Underlying (MemoryT c) x y)
