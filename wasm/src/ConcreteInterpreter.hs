{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ConcreteInterpreter where

import           Data
import           Concrete
import           GenericInterpreter hiding (eval,evalNumericInst,evalParametricInst,invokeExported,store)
import qualified GenericInterpreter as Generic

import           Control.Arrow
import qualified Control.Arrow.Trans as Trans

import           Control.Arrow.Transformer.Stack
import           Control.Arrow.Transformer.StaticGlobalState
import           Control.Arrow.Transformer.Reader
import           Control.Arrow.Transformer.Value
import           Control.Arrow.Transformer.WasmFrame

import           Control.Arrow.Transformer.Concrete.Failure
import           Control.Arrow.Transformer.Concrete.Except
import           Control.Arrow.Transformer.Concrete.Memory
import           Control.Arrow.Transformer.Concrete.Serialize
import           Control.Arrow.Transformer.Concrete.Table

import           Data.Concrete.Error

import qualified Data.Function as Function
import           Data.Text.Lazy (Text)
import qualified Data.Vector as Vec
import           Data.Word

import           Language.Wasm.Interpreter (ModuleInstance)
import qualified Language.Wasm.Interpreter as Wasm
import           Language.Wasm.Structure hiding (exports, Const, Instruction, Function,Expression,Memory,Table)
import           Language.Wasm.Validate (ValidModule)

toVal32 :: Word32 -> Value
toVal32 = Value . Wasm.VI32

toVal64 :: Word64 -> Value
toVal64 = Value . Wasm.VI64

instance ArrowChoice c => IsVal Value (ValueT Value c) where
    type JoinVal y (ValueT Value c) = ()

    i32const = proc w32 -> returnA -< Value $ Wasm.VI32 w32
    i64const = proc w64 -> returnA -< Value $ Wasm.VI64 w64
    iBinOp _eCont = proc (bs,op,Value v1,Value v2) ->
      case (bs,op,v1,v2) of
        (BS32, IAdd, Wasm.VI32 val1, Wasm.VI32 val2) -> returnA -< toVal32 $ val1 + val2
        (BS32, IMul, Wasm.VI32 val1, Wasm.VI32 val2) -> returnA -< toVal32 $ val1 * val2
        (BS32, ISub, Wasm.VI32 val1, Wasm.VI32 val2) -> returnA -< toVal32 $ val1 - val2
        (BS64, IAdd, Wasm.VI64 val1, Wasm.VI64 val2) -> returnA -< toVal64 $ val1 + val2
        (BS64, IMul, Wasm.VI64 val1, Wasm.VI64 val2) -> returnA -< toVal64 $ val1 * val2
        (BS64, ISub, Wasm.VI64 val1, Wasm.VI64 val2) -> returnA -< toVal64 $ val1 - val2
        _ -> returnA -< error "iBinOp: cannot apply binary operator to given arguments."
    iRelOp = proc (bs,op,Value v1, Value v2) ->
      case (bs,op,v1,v2) of
      (BS32, IEq, Wasm.VI32 val1, Wasm.VI32 val2) ->
        returnA -< toVal32 $ if val1 == val2 then 1 else 0
--                    (BS64, ILtU, Wasm.VI64 val1, Wasm.VI64 val2) ->
--                        returnA -< toVal64 $ if val1 < val2 then 1 else 0
      (BS64, IEq, Wasm.VI64 val1, Wasm.VI64 val2) ->
        returnA -< toVal32 $ if val1 == val2 then 1 else 0
      _ -> returnA -< error "iRelOp: cannot apply binary operator to given arguments."

    i32ifNeqz f g = proc (v, x) -> do
      case v of
        Value (Wasm.VI32 0) -> g -< x
        Value (Wasm.VI32 _) -> f -< x
        _                   -> returnA -< error "i32ifNeqz: condition of unexpected type"

    ifHasType f g = proc (v,t,x) -> do
                case (v,t) of
                    (Value (Wasm.VI32 _), I32) -> f -< x
                    (Value (Wasm.VI64 _), I64) -> f -< x
                    (Value (Wasm.VF32 _), F32) -> f -< x
                    (Value (Wasm.VF64 _), F64) -> f -< x
                    _                          -> g -< x

    f32const = error "TODO: implement f32const"
    f64const = error "TODO: implement f64const"
    iUnOp = error "TODO: implement iUnOp"
    i32eqz = error "TODO: implement i32eqz"
    i64eqz = error "TODO: implement i64eqz"
    fUnOp = error "TODO: implement fUnOp"
    fBinOp = error "TODO: implement fBinOp"
    fRelOp = error "TODO: implement fRelOp"
    i32WrapI64 = error "TODO: implement i32WrapI64"
    iTruncFU = error "TODO: implement iTruncFU"
    iTruncFS = error "TODO: implement iTruncFS"
    i64ExtendSI32 = error "TODO: implement i64ExtendSI32"
    i64ExtendUI32 = error "TODO: implement i64ExtendUI32"
    fConvertIU = error "TODO: implement fConvertIU"
    fConvertIS = error "TODO: implement fConvertIS"
    f32DemoteF64 = error "TODO: implement f32DemoteF64"
    f64PromoteF32 = error "TODO: implement f64PromoteF32"
    iReinterpretF = error "TODO: implement iReinterpretF"
    fReinterpretI = error "TODO: implement IReinterpretI"
    listLookup = error "TODO: implement listLookup"

instance (Arrow c) => IsException (Exc Value) Value (ValueT Value c) where
    type JoinExc y (ValueT Value c) = ()
    exception = arr id
    handleException = id

addVal :: Wasm.Value -> Wasm.Value -> Wasm.Value
addVal (Wasm.VI32 v1) (Wasm.VI32 v2) = Wasm.VI32 $ v1 + v2
addVal _ _ = error "addVal: cannot add values. Unexpected types"


--evalNumericInst :: (Instruction Natural) -> [Value] -> Error (Exc Value) Value
--evalNumericInst inst stack =
--    snd $ Trans.run
--      (Generic.evalNumericInst ::
--        ValueT Value
--          (ExceptT (Exc Value)
--            (StackT Value
--              (->))) (Instruction Natural) Value) (AbsList stack,inst)
--
--
----type TransStack = FrameT FrameData Value (StackT Value (->))
----
--evalVariableInst :: (Instruction Natural) -> [Value] -> FrameData -> Vector Value
--            -> GlobalState Value -> ([Value], (Vector Value, (GlobalState Value, ())))
--evalVariableInst inst stack fd locals store =
--    unabs $ Trans.run
--      (Generic.evalVariableInst ::
--        GlobalStateT Value
--          (FrameT FrameData Value
--            (StackT Value
--              (->))) (Instruction Natural) ()) (AbsList stack, (locals, (fd,(store, inst))))
--    where unabs (AbsList x,y) = (x,y)
--
--
--evalParametricInst :: (Instruction Natural) -> [Value] -> ([Value], ())
--evalParametricInst inst stack =
--    unabs $ Trans.run
--      (Generic.evalParametricInst ::
--        ValueT Value
--          (StackT Value
--            (->)) (Instruction Natural) ()) (AbsList stack,inst)
--    where unabs (AbsList x,y) = (x,y)
--

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

type Result = (Error
                             [Char]
                             (JoinVector Value,
                              (Tables,
                               (Memories,
                                (StaticGlobalState Value, Error (Exc Value) (JoinList Value, [Value]))))))


invokeExported :: StaticGlobalState Value
                        -> Memories
                        -> Tables
                        -> ModuleInstance
                        -> Text
                        -> [Value]
                        -> Error
                             [Char]
                             (JoinVector Value,
                              (Tables,
                               (Memories,
                                (StaticGlobalState Value, Error (Exc Value) (JoinList Value, [Value])))))
invokeExported staticS mem tab modInst funcName args =
    let ?fixpointAlgorithm = Function.fix in
    Trans.run
    (Generic.invokeExported ::
      ValueT Value
        (ReaderT Generic.LabelArities
          (StackT Value
            (ExceptT (Generic.Exc Value)
              (StaticGlobalStateT Value
                (MemoryT
                  (SerializeT
                    (TableT
                      (FrameT FrameData Value
                        (FailureT String
                          (->)))))))))) (Text, [Value]) [Value]) (JoinVector Vec.empty,((0,modInst),(tab,(mem,(staticS,([],(Generic.LabelArities [],(funcName,args))))))))


instantiateConcrete :: ValidModule -> IO (Either String (ModuleInstance, StaticGlobalState Value, Memories, Tables))
instantiateConcrete valMod = instantiate valMod Value toMem TableInst
    where
        toMem size lst = MemInst size (Vec.fromList lst)
--    res <- Wasm.instantiate emptyStore emptyImports valMod
--    case res of
--        Right (modInst, store) -> do
--            wasmStore <- storeToGlobalState store
--            return $ Right $ (modInst, wasmStore)
--        Left e                 -> return $ Left e
--
--    where
--        storeToGlobalState (Wasm.Store funcI tableI memI globalI) = do
--            let funcs = generate $ Vec.mapM convertFuncInst funcI
--            mems <- Vec.mapM convertMem memI
--            globs <- Vec.mapM convertGlobals globalI
--            return $ GlobalState funcs --(Vec.map convertFuncs funcI)
--                               (Vec.map TableInst tableI)
--                               mems
--                               globs
--
--        convertMem (Wasm.MemoryInstance (Limit _ n) mem) = do
--            memStore <- readIORef mem
--            size <- ByteArray.getSizeofMutableByteArray memStore
--            list <- sequence $ map (\x -> ByteArray.readByteArray memStore x) [0 .. (size-1)]
--            let sizeConverted = fmap fromIntegral n
--            return $ MemInst sizeConverted $ Vec.fromList list
--
--        convertGlobals (Wasm.GIConst _ v) =  return $ GlobInst Const (Value v)
--        convertGlobals (Wasm.GIMut _ v) = do
--            val <- readIORef v
--            return $ GlobInst Mutable (Value val)
