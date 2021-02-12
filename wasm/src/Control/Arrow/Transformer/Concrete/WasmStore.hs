{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Arrow.Transformer.Concrete.WasmStore where

import           Control.Arrow
import           Control.Arrow.Const
import           Control.Arrow.Except
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.Frame 
import           Control.Arrow.MemAddress
import           Control.Arrow.Memory
import           Control.Arrow.MemSizable
import           Control.Arrow.Reader
import           Control.Arrow.Serialize
import           Control.Arrow.Stack
import           Control.Arrow.State
import           Control.Arrow.Store
import           Control.Arrow.Trans
import           Control.Arrow.WasmStore

import           Control.Arrow.Transformer.Reader
import           Control.Arrow.Transformer.State

import           Control.Category

import           Data.Monoidal (shuffle1)
import           Data.Profunctor
import           Data.Vector (Vector, (!), (//))
import qualified Data.Vector as Vec
import           Data.Word

import           Language.Wasm.Interpreter (ModuleInstance,emptyStore,emptyImports)
import qualified Language.Wasm.Interpreter as Wasm
import           Language.Wasm.Structure hiding (exports)

import           Numeric.Natural (Natural)

import           GenericInterpreter (LoadType,StoreType)

newtype Value = Value Wasm.Value deriving (Show, Eq)

data WasmStore v = WasmStore {
    funcInstances :: Vector FuncInst,
    tableInstances :: Vector TableInst,
    memInstances :: Vector MemInst,
    globalInstances :: Vector v
} deriving (Show, Eq)

emptyWasmStore :: WasmStore v
emptyWasmStore = WasmStore {
    funcInstances = Vec.empty,
    tableInstances = Vec.empty,
    memInstances = Vec.empty,
    globalInstances = Vec.empty
}

data FuncInst =
    FuncInst {
        funcType :: FuncType,
        moduleInstance :: ModuleInstance,
        code :: Function
    }
    | HostInst {
        funcType :: FuncType
        --hostCode :: HostFunction v c
    } deriving (Show,Eq)

newtype TableInst = TableInst Wasm.TableInstance deriving (Show,Eq)
newtype MemInst = MemInst (Vector Word8) deriving (Show,Eq)

newtype WasmStoreT v c x y = WasmStoreT (ReaderT Int (StateT (WasmStore v) c) x y)
    deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowLift,
              ArrowFail e, ArrowExcept e, ArrowConst r, ArrowStore var' val', ArrowRun, ArrowFrame fd val,
              ArrowStack st)--, ArrowState (WasmStore v))

instance (ArrowReader r c) => ArrowReader r (WasmStoreT v c) where
    ask = lift' ask
    local a = lift $ lmap shuffle1 (local (unlift a))

instance (ArrowState s c) => ArrowState s (WasmStoreT v c) where

instance ArrowTrans (WasmStoreT v) where
    -- lift' :: c x y -> WasmStoreT v c x y
    lift' arr = WasmStoreT (lift' (lift' arr))

instance (ArrowChoice c, Profunctor c) => ArrowWasmStore v (WasmStoreT v c) where
    readGlobal = 
        WasmStoreT $ proc i -> do
            WasmStore{globalInstances=vec} <- get -< ()
            returnA -< vec ! i
    writeGlobal =
        WasmStoreT $ proc (i,v) -> do
            store@WasmStore{globalInstances=vec} <- get -< ()
            put -< store{globalInstances=vec // [(i, v)]}
    
    -- funcCont :: ReaderT Int (StateT (WasmStore v) c) ((FuncType, ModuleInstance, Function),x) y
    -- we need ReaderT Int (StateT (WasmStore v) c) (Int, x) y
    readFunction (WasmStoreT funcCont) =
        WasmStoreT $ proc (i,x) -> do
            WasmStore{funcInstances = fs} <- get -< ()
            case fs ! i of
                FuncInst fTy modInst code -> funcCont -< ((fTy,modInst,code),x)
                _                         -> returnA -< error "not yet implemented" --hostCont -< ((fTy,code),x)

    withMemoryInstance (WasmStoreT f) = WasmStoreT $ local f

instance (ArrowChoice c, Profunctor c) => ArrowMemory Word32 (Vector Word8) (WasmStoreT v c) where
    memread (WasmStoreT sCont) (WasmStoreT eCont) = WasmStoreT $ proc (addr, size, x) -> do
        WasmStore{memInstances=mems} <- get -< ()
        currMem <- ask -< ()
        let MemInst vec = mems ! currMem
        let addrI = fromIntegral addr
        case (addrI+size <= length vec) of
            True  -> do
                let content = Vec.slice addrI size vec
                sCont -< (content,x)
            False -> eCont -< x
    memstore (WasmStoreT sCont) (WasmStoreT eCont) = WasmStoreT $ proc (addr, content, x) -> do
        store@WasmStore{memInstances=mems} <- get -< ()
        currMem <- ask -< ()
        let MemInst vec = mems ! currMem
        let addrI = fromIntegral addr
        let size = length content
        case (addrI+size <= length vec) of
            True  -> do
                let ind = Vec.enumFromN addrI size
                put -< (store{memInstances=mems // [(currMem,MemInst $ Vec.update_ vec ind content)]})
                sCont -< x
            False -> eCont -< x

instance (Arrow c, Profunctor c) => ArrowMemAddress Value Natural Word32 (WasmStoreT v c) where
    memaddr = proc (Value (Wasm.VI32 base), off) -> returnA -< (base+ (fromIntegral off))

instance ArrowSerialize v (Vector Word8) ValueType LoadType StoreType (WasmStoreT v c) where

instance ArrowMemSizable v (WasmStoreT v c) where




instance ArrowFix (Underlying (WasmStoreT v c) x y) => ArrowFix (WasmStoreT v c x y) where
    type Fix (WasmStoreT v c x y) = Fix (Underlying (WasmStoreT v c) x y)

deriving instance Show Wasm.TableInstance
deriving instance Eq Wasm.TableInstance
