{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Arrow.Transformer.Concrete.GlobalState where

import           Control.Arrow
import           Control.Arrow.Const
import           Control.Arrow.Logger
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
import           Control.Arrow.GlobalState

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

newtype Value = Value Wasm.Value deriving (Show, Eq)
data Mut = Const | Mutable deriving (Show, Eq)

data GlobalState v = GlobalState {
    funcInstances :: Vector FuncInst,
    tableInstances :: Vector TableInst,
    memInstances :: Vector MemInst,
    globalInstances :: Vector (GlobInst v)
} deriving (Show, Eq)

emptyGlobalState :: GlobalState v
emptyGlobalState = GlobalState {
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
data MemInst = MemInst (Maybe Word32) (Vector Word8) deriving (Show,Eq)
data GlobInst v = GlobInst Mut v deriving (Show, Eq)

newtype GlobalStateT v c x y = GlobalStateT (ReaderT Int (StateT (GlobalState v) c) x y)
    deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowLift,
              ArrowFail e, ArrowExcept e, ArrowConst r, ArrowStore var' val', ArrowRun, ArrowFrame fd val,
              ArrowStack st, ArrowLogger l)--, ArrowState (GlobalState v))

instance (ArrowReader r c) => ArrowReader r (GlobalStateT v c) where
    ask = lift' ask
    local a = lift $ lmap shuffle1 (local (unlift a))

instance (ArrowState s c) => ArrowState s (GlobalStateT v c) where

instance ArrowTrans (GlobalStateT v) where
    -- lift' :: c x y -> GlobalStateT v c x y
    lift' a = GlobalStateT (lift' (lift' a))

instance (ArrowChoice c, Profunctor c) => ArrowGlobalState v (GlobalStateT v c) where
    readGlobal = 
        GlobalStateT $ proc i -> do
            GlobalState{globalInstances=vec} <- get -< ()
            let (GlobInst _ val) = vec ! i
            returnA -< val
    writeGlobal =
        GlobalStateT $ proc (i,v) -> do
            store@GlobalState{globalInstances=vec} <- get -< ()
            let (GlobInst m _) = vec ! i
            if m == Const
                then returnA -< error $ "writing to constant global " ++ (show i)
                else put -< store{globalInstances=vec // [(i, GlobInst m v)]}
    
    -- funcCont :: ReaderT Int (StateT (GlobalState v) c) ((FuncType, ModuleInstance, Function),x) y
    -- we need ReaderT Int (StateT (GlobalState v) c) (Int, x) y
    readFunction (GlobalStateT funcCont) =
        GlobalStateT $ proc (i,x) -> do
            GlobalState{funcInstances = fs} <- get -< ()
            case fs ! i of
                FuncInst fTy modInst code -> funcCont -< ((fTy,modInst,code),x)
                _                         -> returnA -< error "not yet implemented" --hostCont -< ((fTy,code),x)

    withMemoryInstance (GlobalStateT f) = GlobalStateT $ local f

instance (ArrowChoice c, Profunctor c) => ArrowMemory Word32 (Vector Word8) (GlobalStateT v c) where
    memread (GlobalStateT sCont) (GlobalStateT eCont) = GlobalStateT $ proc (addr, size, x) -> do
        GlobalState{memInstances=mems} <- get -< ()
        currMem <- ask -< ()
        let MemInst _ vec = mems ! currMem
        let addrI = fromIntegral addr
        case (addrI+size <= length vec) of
            True  -> do
                let content = Vec.slice addrI size vec
                sCont -< (content,x)
            False -> eCont -< x
    memstore (GlobalStateT sCont) (GlobalStateT eCont) = GlobalStateT $ proc (addr, content, x) -> do
        store@GlobalState{memInstances=mems} <- get -< ()
        currMem <- ask -< ()
        let MemInst s vec = mems ! currMem
        let addrI = fromIntegral addr
        let size = length content
        case (addrI+size <= length vec) of
            True  -> do
                let ind = Vec.enumFromN addrI size
                put -< (store{memInstances=mems // [(currMem,MemInst s $ Vec.update_ vec ind content)]})
                sCont -< x
            False -> eCont -< x

instance (Arrow c, Profunctor c) => ArrowMemAddress Value Natural Word32 (GlobalStateT v c) where
    memaddr = proc (Value (Wasm.VI32 base), off) -> returnA -< (base+ (fromIntegral off))

instance ArrowSerialize v (Vector Word8) ValueType LoadType StoreType (GlobalStateT v c) where

instance ArrowMemSizable v (GlobalStateT v c) where




instance ArrowFix (Underlying (GlobalStateT v c) x y) => ArrowFix (GlobalStateT v c x y) where
    type Fix (GlobalStateT v c x y) = Fix (Underlying (GlobalStateT v c) x y)

deriving instance Show Wasm.TableInstance
deriving instance Eq Wasm.TableInstance
