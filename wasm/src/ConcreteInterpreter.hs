{-# LANGUAGE Arrows #-}
--{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module ConcreteInterpreter where

import           Frame
import           GenericInterpreter hiding (eval,evalNumericInst,evalVariableInstr,evalParametricInst,invokeExported)
import qualified GenericInterpreter as Generic
--import           Stack

import           Control.Arrow
import           Control.Arrow.Const
import           Control.Arrow.Except
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.Reader
import           Control.Arrow.Stack
import           Control.Arrow.State
import           Control.Arrow.Store
import qualified Control.Arrow.Trans as Trans
import           Control.Arrow.Transformer.Kleisli
import           Control.Arrow.Transformer.Stack
import           Control.Arrow.Trans
import           Control.Arrow.Transformer.Reader
import           Control.Arrow.Transformer.Value

import           Control.Arrow.Transformer.Concrete.Failure
import           Control.Arrow.Transformer.Concrete.Except
import           Control.Arrow.Transformer.State

import           Control.Category

import           Data.Concrete.Error

import qualified Data.Function as Function
import           Data.Profunctor
import           Data.Text.Lazy (Text)
import           Data.Vector (Vector, (!), (//))
import qualified Data.Vector as Vec
import           Data.Word

import           Language.Wasm.Interpreter hiding (Value)
import qualified Language.Wasm.Interpreter as Wasm
import           Language.Wasm.Structure hiding (exports)

import           Numeric.Natural (Natural)


-- memory instance: vec(byte) + optional max size
-- bytes are modeled as Word8
-- addresses are modeled as Word32
--newtype WasmMemoryT c x y = WasmMemoryT (StateT (Vector Word8) c x y)
--    deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowLift,
--              ArrowFail e, ArrowExcept e, ArrowConst r, ArrowStore var' val', ArrowRun, ArrowFrame fd val,
--              ArrowStack st, ArrowState (Vector Word8), ArrowReader r, ArrowWasmStore v2)
--
--instance ArrowTrans (WasmMemoryT) where
--    -- lift' :: c x y -> WasmStoreT v c x y
--    lift' arr = WasmMemoryT (lift' arr)
--
--
--instance (ArrowChoice c, Profunctor c) => ArrowMemory Word32 (Vector Word8) (WasmMemoryT c) where
--    memread sCont eCont = proc (addr, size, x) -> do
--        vec <- get -< ()
--        let addrI = fromIntegral addr
--        case (addrI+size <= length vec) of
--            True -> do
--                let content = Vec.slice addrI size vec
--                sCont -< (content,x)
--            False -> eCont -< x
--    memstore sCont eCont = proc (addr, content, x) -> do
--        vec <- get -< ()
--        let addrI = fromIntegral addr
--        let size = length content
--        case (addrI+size <= length vec) of
--            True  -> do
--                let ind = Vec.enumFromN addrI size
--                put -< (Vec.update_ vec ind content)
--                sCont -< x
--            False -> eCont -< x
--
--instance (Arrow c, Profunctor c) => ArrowMemAddress Value Natural Word32 (WasmMemoryT c) where
--    memaddr = proc (Value (VI32 base), off) -> returnA -< (base+ (fromIntegral off))
--
--instance ArrowSerialize Value (Vector Word8) ValueType LoadType StoreType (WasmMemoryT c) where
--
--instance ArrowMemSizable Value (WasmMemoryT c) where
--
--instance ArrowFix (Underlying (WasmMemoryT c) x y) => ArrowFix (WasmMemoryT c x y) where
--    type Fix (WasmMemoryT c x y) = Fix (Underlying (WasmMemoryT c) x y)
--
--
data WasmStore v = WasmStore {
    funcInstances :: Vector (FuncInst v),
    tableInstances :: Vector TableInst,
    memInstances :: Vector MemInst,
    globalInstances :: Vector v
} deriving (Show)

emptyWasmStore :: WasmStore v
emptyWasmStore = WasmStore {
    funcInstances = Vec.empty,
    tableInstances = Vec.empty,
    memInstances = Vec.empty,
    globalInstances = Vec.empty
}

data FuncInst v =
    FuncInst {
        funcType :: FuncType,
        moduleInstance :: ModuleInstance,
        code :: Function
    }
    | HostInst {
        funcType :: FuncType
        --hostCode :: HostFunction v c
    } deriving (Show)

data TableInst = TableInst deriving (Show)
newtype MemInst = MemInst (Vector Word8) deriving (Show)

newtype WasmStoreT v c x y = WasmStoreT (ReaderT Int (StateT (WasmStore v) c) x y)
    deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowLift,
              ArrowFail e, ArrowExcept e, ArrowConst r, ArrowStore var' val', ArrowRun, ArrowFrame fd val,
              ArrowStack st)--, ArrowState (WasmStore v))

instance (ArrowReader r c) => ArrowReader r (WasmStoreT v c) where

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
    memaddr = proc (Value (VI32 base), off) -> returnA -< (base+ (fromIntegral off))

instance ArrowSerialize Value (Vector Word8) ValueType LoadType StoreType (WasmStoreT v c) where

instance ArrowMemSizable Value (WasmStoreT v c) where




instance ArrowFix (Underlying (WasmStoreT v c) x y) => ArrowFix (WasmStoreT v c x y) where
    type Fix (WasmStoreT v c x y) = Fix (Underlying (WasmStoreT v c) x y)

newtype Value = Value Wasm.Value deriving (Show, Eq)


instance (ArrowChoice c) => IsVal Value (ValueT Value c) where
    i32const = proc w32 -> returnA -< Value $ VI32 w32   
    i64const = proc w64 -> returnA -< Value $ VI64 w64
    iBinOp = proc (bs,op,Value v1,Value v2) ->
                case bs of 
                    BS32 -> do
                                case op of
                                    IAdd -> returnA -< Just $ Value $ addVal v1 v2
    i32ifNeqz f g = proc (v, x) -> do
                      case v of
                        Value (VI32 0) -> g -< x
                        Value (VI32 _) -> f -< x
                        _              -> returnA -< error "validation failure"

addVal :: Wasm.Value -> Wasm.Value -> Wasm.Value
addVal (VI32 v1) (VI32 v2) = VI32 $ v1 + v2


evalNumericInst :: (Instruction Natural) -> [Value] -> Error (Exc Value) Value
evalNumericInst inst stack =
    snd $ Trans.run
      (Generic.evalNumericInst ::
        ValueT Value
          (ExceptT (Exc Value)
            (StackT Value
              (->))) (Instruction Natural) Value) (stack,inst)


--type TransStack = FrameT FrameData Value (StackT Value (->))
--
evalVariableInst :: (Instruction Natural) -> [Value] -> FrameData -> Vector Value 
            -> WasmStore Value -> Int -> ([Value], (Vector Value, (WasmStore Value, ())))
evalVariableInst inst stack fd locals store currentMem =
    Trans.run
      (Generic.evalVariableInst ::
        WasmStoreT Value
          (FrameT FrameData Value
            (StackT Value
              (->))) (Instruction Natural) ()) (stack, (locals, (fd,(store, (currentMem,inst)))))


evalParametricInst :: (Instruction Natural) -> [Value] -> ([Value], ())
evalParametricInst inst stack =
    Trans.run
      (Generic.evalParametricInst ::
        ValueT Value
          (StackT Value
            (->)) (Instruction Natural) ()) (stack,inst)


eval :: [Instruction Natural] -> [Value] -> Generic.Read -> Vector Value -> FrameData ->
        WasmStore Value -> Int ->
                                 ([Value], -- stack
                                   Error (Generic.Exc Value)
                                         (Vector Value, -- state of FrameT
                                          (WasmStore Value, -- state of WasmStoreT
                                           ())))
eval inst stack r locals fd wasmStore currentMem =
    let ?fixpointAlgorithm = Function.fix in
    Trans.run
    (Generic.eval ::
      ValueT Value
        (WasmStoreT Value
          (FrameT FrameData Value
            (ReaderT Generic.Read
              (ExceptT (Generic.Exc Value)
                (StackT Value
                  (->)))))) [Instruction Natural] ()) (stack,(r,(locals,(fd,(wasmStore,(currentMem,inst))))))



invokeExported :: WasmStore Value
                        -> ModuleInstance
                        -> Text
                        -> [Value]
                        -> Error
                             [Char]
                             ([Value],
                              Error (Exc Value) (Vector Value, (WasmStore Value, [Value])))
invokeExported store modInst funcName args =
    let ?fixpointAlgorithm = Function.fix in
    Trans.run
    (Generic.invokeExported ::
      ValueT Value
        (WasmStoreT Value
          (FrameT FrameData Value
            (ReaderT Generic.Read
              (ExceptT (Generic.Exc Value)
                (StackT Value
                  (FailureT String
                    (->))))))) (Text, [Value]) [Value]) ([],(Generic.Read [],(Vec.empty,((0,modInst),(store,(0,(funcName,args)))))))
