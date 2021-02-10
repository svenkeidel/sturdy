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
import           Data.Monoidal (shuffle1)
import           Data.Profunctor
import           Data.Text.Lazy (Text)
import           Data.Vector (Vector, (!), (//))
import qualified Data.Vector as Vec
import           Data.Word

import           Language.Wasm.Interpreter (ModuleInstance,emptyStore,emptyImports)
import qualified Language.Wasm.Interpreter as Wasm
import           Language.Wasm.Structure hiding (exports)
import           Language.Wasm.Validate (ValidModule)

import           Numeric.Natural (Natural)

import           System.IO.Unsafe (unsafePerformIO)

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
--    memaddr = proc (Value (Wasm.VI32 base), off) -> returnA -< (base+ (fromIntegral off))
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

instance ArrowSerialize Value (Vector Word8) ValueType LoadType StoreType (WasmStoreT v c) where

instance ArrowMemSizable Value (WasmStoreT v c) where




instance ArrowFix (Underlying (WasmStoreT v c) x y) => ArrowFix (WasmStoreT v c x y) where
    type Fix (WasmStoreT v c x y) = Fix (Underlying (WasmStoreT v c) x y)

newtype Value = Value Wasm.Value deriving (Show, Eq)


instance (ArrowChoice c) => IsVal Value (ValueT Value c) where
    i32const = proc w32 -> returnA -< Value $ Wasm.VI32 w32
    i64const = proc w64 -> returnA -< Value $ Wasm.VI64 w64
    iBinOp = proc (bs,op,Value v1,Value v2) ->
                case bs of 
                    BS32 -> do
                                case op of
                                    IAdd -> returnA -< Just $ Value $ addVal v1 v2
    iRelOp = proc (bs,op,Value v1, Value v2) ->
                case bs of
                    BS32 -> do
                                case op of
                                   IEq -> returnA -< Value $ if v1 == v2 then (Wasm.VI32 1) else (Wasm.VI32 0)
                    BS64 -> do
                                case op of
                                   IEq -> returnA -< Value $ if v1 == v2 then (Wasm.VI32 1) else (Wasm.VI32 0)


    i32ifNeqz f g = proc (v, x) -> do
                      case v of
                        Value (Wasm.VI32 0) -> g -< x
                        Value (Wasm.VI32 _) -> f -< x
                        _              -> returnA -< error "validation failure"

    ifHasType f g = proc (v,t,x) -> do
                case (v,t) of
                    (Value (Wasm.VI32 _), I32) -> f -< x
                    (Value (Wasm.VI64 _), I64) -> f -< x
                    (Value (Wasm.VF32 _), F32) -> f -< x
                    (Value (Wasm.VF64 _), F64) -> f -< x
                    _                          -> g -< x

addVal :: Wasm.Value -> Wasm.Value -> Wasm.Value
addVal (Wasm.VI32 v1) (Wasm.VI32 v2) = Wasm.VI32 $ v1 + v2


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


eval :: [Instruction Natural] -> [Value] -> Generic.LabelArities -> Vector Value -> FrameData ->
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
            (ReaderT Generic.LabelArities
              (ExceptT (Generic.Exc Value)
                (StackT Value
                  (->)))))) [Instruction Natural] ()) (stack,(r,(locals,(fd,(wasmStore,(currentMem,inst))))))



invokeExported :: WasmStore Value
                        -> ModuleInstance
                        -> Text
                        -> [Value]
                        -> Error
                             [Char]
                             (_)
invokeExported store modInst funcName args =
    let ?fixpointAlgorithm = Function.fix in
    Trans.run
    (Generic.invokeExported ::
      ValueT Value
        (ReaderT Generic.LabelArities
          (StackT Value
            (ExceptT (Generic.Exc Value)
              (WasmStoreT Value
                (FrameT FrameData Value
                  (FailureT String
                    (->))))))) (Text, [Value]) [Value]) (Vec.empty,((0,modInst),(store,(0,([],(Generic.LabelArities [],(funcName,args)))))))


instantiate :: ValidModule -> IO (Either String (ModuleInstance, WasmStore Value))
instantiate valMod = do
    res <- Wasm.instantiate emptyStore emptyImports valMod
    case res of
        Right (modInst, store) -> return $ Right $ (modInst, storeToWasmStore store)
        Left e                 -> return $ Left e

    where
        storeToWasmStore (Wasm.Store funcI tableI memI globalI) =
            WasmStore (Vec.map convertFuncs funcI)
                      (Vec.map TableInst tableI)
                      (Vec.map convertMem memI)
                      (convertGlobals globalI)
        convertFuncs (Wasm.FunctionInstance t m c) = FuncInst t m c
        convertFuncs (Wasm.HostInstance t _) = HostInst t
        convertMem (Wasm.MemoryInstance _ _) = MemInst Vec.empty -- TODO
        convertGlobals _ = Vec.empty -- TODO


deriving instance Show Wasm.TableInstance
deriving instance Eq Wasm.TableInstance
