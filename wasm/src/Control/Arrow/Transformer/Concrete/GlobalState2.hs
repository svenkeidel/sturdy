{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Arrow.Transformer.Concrete.GlobalState2 where

import           Control.Arrow
import           Control.Arrow.Const
import           Control.Arrow.Logger
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
import           Control.Arrow.Store
import           Control.Arrow.Trans
import           Control.Arrow.GlobalState
import           Control.Arrow.WasmFrame

import           Control.Arrow.Transformer.Reader
import           Control.Arrow.Transformer.State

import           Control.Category

import           Data.Monoidal (shuffle1)
import           Data.Profunctor
import           Data.Vector (Vector, (!), (//))
import qualified Data.Vector as Vec
import           Data.Word

import           Language.Wasm.Interpreter (ModuleInstance)
import qualified Language.Wasm.Interpreter as Wasm
import           Language.Wasm.Structure hiding (exports, Const)

import           Numeric.Natural (Natural)

import           GenericInterpreter (LoadType,StoreType)
import           Concrete

newtype GlobalState2T v c x y = GlobalState2T (StateT (GlobalState v) c x y)
    deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowLift,
              ArrowFail e, ArrowExcept e, ArrowConst r, ArrowStore var' val', ArrowRun, ArrowFrame fd val,
              ArrowStack st, ArrowLogger l, ArrowMemory m addr bytes)--, ArrowState (GlobalState v))

instance (ArrowReader r c) => ArrowReader r (GlobalState2T v c) where
    ask = lift' ask
    local a = lift $ lmap shuffle1 (local (unlift a))

instance (ArrowState s c) => ArrowState s (GlobalState2T v c) where
    -- TODO

instance ArrowTrans (GlobalState2T v) where
    -- lift' :: c x y -> GlobalStateT v c x y
    lift' a = GlobalState2T (lift' a)

instance (ArrowChoice c, Profunctor c) => ArrowGlobalState v MemInst (GlobalState2T v c) where
    readGlobal = 
        GlobalState2T $ proc i -> do
            GlobalState{globalInstances=vec} <- get -< ()
            let (GlobInst _ val) = vec ! i
            returnA -< val
    writeGlobal =
        GlobalState2T $ proc (i,v) -> do
            store@GlobalState{globalInstances=vec} <- get -< ()
            let (GlobInst m _) = vec ! i
            if m == Const
                then returnA -< error $ "writing to constant global " ++ (show i)
                else put -< store{globalInstances=vec // [(i, GlobInst m v)]}
    
    -- funcCont :: ReaderT Int (StateT (GlobalState v) c) ((FuncType, ModuleInstance, Function),x) y
    -- we need ReaderT Int (StateT (GlobalState v) c) (Int, x) y
    readFunction (GlobalState2T funcCont) =
        GlobalState2T $ proc (i,x) -> do
            GlobalState{funcInstances = fs} <- get -< ()
            case fs ! i of
                FuncInst fTy modInst code -> funcCont -< ((fTy,modInst,code),x)
                _                         -> returnA -< error "not yet implemented" --hostCont -< ((fTy,code),x)

    --withMemoryInstance (GlobalStateT f) = GlobalStateT $ local f

    fetchMemory = GlobalState2T $ proc i -> do
        GlobalState{memInstances=mems} <- get -< ()
        returnA -< mems ! i
    
    storeMemory = GlobalState2T $ proc (i,m) -> do
        gs@GlobalState{memInstances=mems} <- get -< ()
        put -< gs{memInstances=mems // [(i,m)]} 

instance (Arrow c, Profunctor c) => ArrowMemAddress Value Natural Word32 (GlobalState2T v c) where
    memaddr = proc (Value (Wasm.VI32 base), off) -> returnA -< (base+ (fromIntegral off))

instance ArrowSerialize v (Vector Word8) ValueType LoadType StoreType (GlobalState2T v c) where

instance ArrowMemSizable v (GlobalState2T v c) where




instance ArrowFix (Underlying (GlobalState2T v c) x y) => ArrowFix (GlobalState2T v c x y) where
    type Fix (GlobalState2T v c x y) = Fix (Underlying (GlobalState2T v c) x y)
