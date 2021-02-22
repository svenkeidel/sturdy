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

import           GenericInterpreter hiding (eval,evalNumericInst,evalParametricInst,invokeExported,store)
import qualified GenericInterpreter as Generic

import           Control.Arrow
import qualified Control.Arrow.Trans as Trans

import           Control.Arrow.Transformer.DebuggableStack
import           Control.Arrow.Transformer.Logger
import           Control.Arrow.Transformer.Stack
import           Control.Arrow.Transformer.Reader
import           Control.Arrow.Transformer.Value

import           Control.Arrow.Transformer.Concrete.Failure
import           Control.Arrow.Transformer.Concrete.Frame
import           Control.Arrow.Transformer.Concrete.Except
import           Control.Arrow.Transformer.Concrete.GlobalState

import           Data.Concrete.Error

import qualified Data.Function as Function
import           Data.IORef
import qualified Data.Primitive.ByteArray as ByteArray
import           Data.Text.Lazy (Text)
import           Data.Vector (Vector)
import qualified Data.Vector as Vec
import           Data.Word

import           Language.Wasm.Interpreter (ModuleInstance,emptyStore,emptyImports)
import qualified Language.Wasm.Interpreter as Wasm
import           Language.Wasm.Structure hiding (exports, Const)
import           Language.Wasm.Validate (ValidModule)

import           Numeric.Natural (Natural)

-- memory instance: vec(byte) + optional max size
-- bytes are modeled as Word8
-- addresses are modeled as Word32
--newtype WasmMemoryT c x y = WasmMemoryT (StateT (Vector Word8) c x y)
--    deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowLift,
--              ArrowFail e, ArrowExcept e, ArrowConst r, ArrowStore var' val', ArrowRun, ArrowFrame fd val,
--              ArrowStack st, ArrowState (Vector Word8), ArrowReader r, ArrowGlobalState v2)
--
--instance ArrowTrans (WasmMemoryT) where
--    -- lift' :: c x y -> GlobalStateT v c x y
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

toVal32 :: Word32 -> Value
toVal32 = Value . Wasm.VI32

toVal64 :: Word64 -> Value
toVal64 = Value . Wasm.VI64


instance (ArrowChoice c) => IsVal Value (ValueT Value c) where
    i32const = proc w32 -> returnA -< Value $ Wasm.VI32 w32
    i64const = proc w64 -> returnA -< Value $ Wasm.VI64 w64
    iBinOp = proc (bs,op,Value v1,Value v2) ->
                case (bs,op,v1,v2) of
                    (BS32, IAdd, Wasm.VI32 val1, Wasm.VI32 val2) -> returnA -< Just $ toVal32 $ val1 + val2
                    (BS32, IMul, Wasm.VI32 val1, Wasm.VI32 val2) -> returnA -< Just $ toVal32 $ val1 * val2
                    (BS32, ISub, Wasm.VI32 val1, Wasm.VI32 val2) -> returnA -< Just $ toVal32 $ val1 - val2
                    (BS64, IAdd, Wasm.VI64 val1, Wasm.VI64 val2) -> returnA -< Just $ toVal64 $ val1 + val2
                    (BS64, IMul, Wasm.VI64 val1, Wasm.VI64 val2) -> returnA -< Just $ toVal64 $ val1 * val2
                    (BS64, ISub, Wasm.VI64 val1, Wasm.VI64 val2) -> returnA -< Just $ toVal64 $ val1 - val2
    iRelOp = proc (bs,op,Value v1, Value v2) ->
                case (bs,op,v1,v2) of
                    (BS32, IEq, Wasm.VI32 val1, Wasm.VI32 val2) ->
                        returnA -< toVal32 $ if val1 == val2 then 1 else 0
--                    (BS64, ILtU, Wasm.VI64 val1, Wasm.VI64 val2) ->
--                        returnA -< toVal64 $ if val1 < val2 then 1 else 0
                    (BS64, IEq, Wasm.VI64 val1, Wasm.VI64 val2) ->
                        returnA -< toVal32 $ if val1 == val2 then 1 else 0


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
            -> GlobalState Value -> ([Value], (Vector Value, (GlobalState Value, ())))
evalVariableInst inst stack fd locals store =
    Trans.run
      (Generic.evalVariableInst ::
        GlobalStateT Value
          (FrameT FrameData Value
            (StackT Value
              (->))) (Instruction Natural) ()) (stack, (locals, (fd,(store, inst))))


evalParametricInst :: (Instruction Natural) -> [Value] -> ([Value], ())
evalParametricInst inst stack =
    Trans.run
      (Generic.evalParametricInst ::
        ValueT Value
          (StackT Value
            (->)) (Instruction Natural) ()) (stack,inst)


--eval :: [Instruction Natural] -> [Value] -> Generic.LabelArities -> Vector Value -> FrameData ->
--        GlobalState Value -> Int ->
--                                 ([Value], -- stack
--                                   Error (Generic.Exc Value)
--                                         (Vector Value, -- state of FrameT
--                                          (GlobalState Value, -- state of GlobalStateT
--                                           ())))
--eval inst stack r locals fd wasmStore currentMem =
--    let ?fixpointAlgorithm = Function.fix in
--    Trans.run
--    (Generic.eval ::
--      ValueT Value
--        (GlobalStateT Value
--          (FrameT FrameData Value
--            (ReaderT Generic.LabelArities
--              (ExceptT (Generic.Exc Value)
--                (StackT Value
--                  (->)))))) [Instruction Natural] ()) (stack,(r,(locals,(fd,(wasmStore,(currentMem,inst))))))



invokeExported :: GlobalState Value
                        -> ModuleInstance
                        -> Text
                        -> [Value]
                        -> ([String], Error
                             [Char]
                             (Vector Value,
                              (GlobalState Value, Error (Exc Value) ([Value], [Value]))))
invokeExported store modInst funcName args =
    let ?fixpointAlgorithm = Function.fix in
    Trans.run
    (Generic.invokeExported ::
      ValueT Value
        (ReaderT Generic.LabelArities
          (DebuggableStackT Value
            (ExceptT (Generic.Exc Value)
              (GlobalStateT Value
                (FrameT FrameData Value
                  (FailureT String
                    (LoggerT String
                      (->)))))))) (Text, [Value]) [Value]) ([],(Vec.empty,((0,modInst),(store,([],(Generic.LabelArities [],(funcName,args)))))))


instantiate :: ValidModule -> IO (Either String (ModuleInstance, GlobalState Value))
instantiate valMod = do
    res <- Wasm.instantiate emptyStore emptyImports valMod
    case res of
        Right (modInst, store) -> do
            wasmStore <- storeToGlobalState store
            return $ Right $ (modInst, wasmStore)
        Left e                 -> return $ Left e

    where
        storeToGlobalState (Wasm.Store funcI tableI memI globalI) = do
            mems <- Vec.mapM convertMem memI
            globs <- Vec.mapM convertGlobals globalI
            return $ GlobalState (Vec.map convertFuncs funcI)
                               (Vec.map TableInst tableI)
                               mems
                               globs

        convertFuncs (Wasm.FunctionInstance t m c) = FuncInst t m c
        convertFuncs (Wasm.HostInstance t _) = HostInst t

        convertMem (Wasm.MemoryInstance (Limit _ n) mem) = do
            memStore <- readIORef mem
            size <- ByteArray.getSizeofMutableByteArray memStore
            list <- sequence $ map (\x -> ByteArray.readByteArray memStore x) [0 .. (size-1)]
            let sizeConverted = fmap fromIntegral n
            return $ MemInst sizeConverted $ Vec.fromList list

        convertGlobals (Wasm.GIConst _ v) =  return $ GlobInst Const (Value v)
        convertGlobals (Wasm.GIMut _ v) = do
            val <- readIORef v
            return $ GlobInst Mutable (Value val)
