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

import           Data hiding (I32Eqz)
import           Concrete
import           GenericInterpreter hiding (eval,evalNumericInst,evalParametricInst,invokeExported,store)
import qualified GenericInterpreter as Generic

import           Control.Arrow
import qualified Control.Arrow.Trans as Trans
import           Control.Arrow.Except

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
import           Data.Int
import           Data.Word
import           Data.Bits

import           Language.Wasm.Interpreter (ModuleInstance)
import qualified Language.Wasm.Interpreter as Wasm
import           Language.Wasm.Structure hiding (exports, Const, Instruction, Function,Expression,Memory,Table)
import           Language.Wasm.Validate (ValidModule)

trap :: IsException (Exc v) v c => c String x
trap = throw <<< exception <<^ Trap

instance (IsException (Exc Value) Value (ValueT Value c), ArrowChoice c) => IsVal Value (ValueT Value c) where
    type JoinVal y (ValueT Value c) = ()

    i32const = proc w32 -> returnA -< int32 w32
    i64const = proc w64 -> returnA -< int64 w64

    iUnOp = proc (bs,op,Value v0) -> case (bs,op,v0) of
        (BS32, IClz,    Wasm.VI32 v) -> returnA -< int32 $ fromIntegral $ countLeadingZeros v
        (BS32, ICtz,    Wasm.VI32 v) -> returnA -< int32 $ fromIntegral $ countTrailingZeros v
        (BS32, IPopcnt, Wasm.VI32 v) -> returnA -< int32 $ fromIntegral $ popCount v
        (BS64, IClz,    Wasm.VI64 v) -> returnA -< int64 $ fromIntegral $ countLeadingZeros v
        (BS64, ICtz,    Wasm.VI64 v) -> returnA -< int64 $ fromIntegral $ countTrailingZeros v
        (BS64, IPopcnt, Wasm.VI64 v) -> returnA -< int64 $ fromIntegral $ popCount v
        _ -> returnA -< error "iUnOp: cannot apply operator to arguements"
    iBinOp _eCont = proc (bs,op,Value v1,Value v2) -> case (bs,op,v1,v2) of
        (BS32, IAdd, Wasm.VI32 val1, Wasm.VI32 val2) -> returnA -< int32 $ val1 + val2
        (BS32, ISub, Wasm.VI32 val1, Wasm.VI32 val2) -> returnA -< int32 $ val1 - val2
        (BS32, IMul, Wasm.VI32 val1, Wasm.VI32 val2) -> returnA -< int32 $ val1 * val2
        (BS32, IDivU, Wasm.VI32 val1, Wasm.VI32 val2) ->
          if val2 == 0
          then trap -< "divide by 0"
          else returnA -< int32 $ val1 `quot` val2
        (BS32, IDivS, Wasm.VI32 val1, Wasm.VI32 val2) ->
          if val2 == 0 || (val1 == 0x80000000 && val2 == 0xFFFFFFFF)
          then trap -< "divide by 0"
          else returnA -< int32 $ asWord32 (asInt32 val1 `quot` asInt32 val2)
        (BS32, IRemU, Wasm.VI32 val1, Wasm.VI32 val2) ->
          if val2 == 0
          then trap -< "divide by 0"
          else returnA -< int32 $ val1 `rem` val2
        (BS32, IRemS, Wasm.VI32 val1, Wasm.VI32 val2) ->
          if val2 == 0
          then trap -< "divide by 0"
          else returnA -< int32 $ asWord32 (asInt32 val1 `rem` asInt32 val2)
        (BS32, IAnd,  Wasm.VI32 val1, Wasm.VI32 val2) -> returnA -< int32 $ val1 .&. val2
        (BS32, IOr,   Wasm.VI32 val1, Wasm.VI32 val2) -> returnA -< int32 $ val1 .|. val2
        (BS32, IXor,  Wasm.VI32 val1, Wasm.VI32 val2) -> returnA -< int32 $ val1 `xor` val2
        (BS32, IShl,  Wasm.VI32 val1, Wasm.VI32 val2) -> returnA -< int32 $ val1 `shiftL` (fromIntegral val2 `rem` 32)
        (BS32, IShrU, Wasm.VI32 val1, Wasm.VI32 val2) -> returnA -< int32 $ val1 `shiftR` (fromIntegral val2 `rem` 32)
        (BS32, IShrS, Wasm.VI32 val1, Wasm.VI32 val2) -> returnA -< int32 $ asWord32 $ asInt32 val1 `shiftR` (fromIntegral val2 `rem` 32)
        (BS32, IRotl, Wasm.VI32 val1, Wasm.VI32 val2) -> returnA -< int32 $ val1 `rotateL` fromIntegral val2
        (BS32, IRotr, Wasm.VI32 val1, Wasm.VI32 val2) -> returnA -< int32 $ val1 `rotateR` fromIntegral val2

        (BS64, IAdd,  Wasm.VI64 val1, Wasm.VI64 val2) -> returnA -< int64 $ val1 + val2
        (BS64, ISub,  Wasm.VI64 val1, Wasm.VI64 val2) -> returnA -< int64 $ val1 - val2
        (BS64, IMul,  Wasm.VI64 val1, Wasm.VI64 val2) -> returnA -< int64 $ val1 * val2
        (BS64, IDivU, Wasm.VI64 val1, Wasm.VI64 val2) ->
          if val2 == 0
          then trap -< "divide by 0"
          else returnA -< int64 $ val1 `quot` val2
        (BS64, IDivS, Wasm.VI64 val1, Wasm.VI64 val2) ->
          if val2 == 0 || (val1 == 0x8000000000000000 && val2 == 0xFFFFFFFFFFFFFFFF)
          then trap -< "divide by 0"
          else returnA -< int64 $ asWord64 (asInt64 val1 `quot` asInt64 val2)
        (BS64, IRemU, Wasm.VI64 val1, Wasm.VI64 val2) ->
          if val2 == 0
          then trap -< "divide by 0"
          else returnA -< int64 $ val1 `rem` val2
        (BS64, IRemS, Wasm.VI64 val1, Wasm.VI64 val2) ->
          if val2 == 0
          then trap -< "divide by 0"
          else returnA -< int64 $ asWord64 (asInt64 val1 `rem` asInt64 val2)
        (BS64, IAnd,  Wasm.VI64 val1, Wasm.VI64 val2) -> returnA -< int64 $ val1 .&. val2
        (BS64, IOr,   Wasm.VI64 val1, Wasm.VI64 val2) -> returnA -< int64 $ val1 .|. val2
        (BS64, IXor,  Wasm.VI64 val1, Wasm.VI64 val2) -> returnA -< int64 $ val1 `xor` val2
        (BS64, IShl,  Wasm.VI64 val1, Wasm.VI64 val2) -> returnA -< int64 $ val1 `shiftL` (fromIntegral val2 `rem` 64)
        (BS64, IShrU, Wasm.VI64 val1, Wasm.VI64 val2) -> returnA -< int64 $ val1 `shiftR` (fromIntegral val2 `rem` 64)
        (BS64, IShrS, Wasm.VI64 val1, Wasm.VI64 val2) -> returnA -< int64 $ asWord64 $ asInt64 val1 `shiftR` (fromIntegral val2 `rem` 64)
        (BS64, IRotl, Wasm.VI64 val1, Wasm.VI64 val2) -> returnA -< int64 $ val1 `rotateL` fromIntegral val2
        (BS64, IRotr, Wasm.VI64 val1, Wasm.VI64 val2) -> returnA -< int64 $ val1 `rotateR` fromIntegral val2
        _ -> returnA -< error "iBinOp: cannot apply binary operator to given arguments."
    iRelOp = proc (bs,op,Value v1, Value v2) -> case (bs,op,v1,v2) of
        (BS32, IEq,  Wasm.VI32 val1, Wasm.VI32 val2) -> returnA -< int32 $ if val1 == val2 then 1 else 0
        (BS32, INe,  Wasm.VI32 val1, Wasm.VI32 val2) -> returnA -< int32 $ if val1 /= val2 then 1 else 0
        (BS32, ILtU, Wasm.VI32 val1, Wasm.VI32 val2) -> returnA -< int32 $ if val1 <  val2 then 1 else 0
        (BS32, ILtS, Wasm.VI32 val1, Wasm.VI32 val2) -> returnA -< int32 $ if asInt32 val1 < asInt32 val2 then 1 else 0
        (BS32, IGtU, Wasm.VI32 val1, Wasm.VI32 val2) -> returnA -< int32 $ if val1 >  val2 then 1 else 0
        (BS32, IGtS, Wasm.VI32 val1, Wasm.VI32 val2) -> returnA -< int32 $ if asInt32 val1 > asInt32 val2 then 1 else 0
        (BS32, ILeU, Wasm.VI32 val1, Wasm.VI32 val2) -> returnA -< int32 $ if val1 <= val2 then 1 else 0
        (BS32, ILeS, Wasm.VI32 val1, Wasm.VI32 val2) -> returnA -< int32 $ if asInt32 val1 <= asInt32 val2 then 1 else 0
        (BS32, IGeU, Wasm.VI32 val1, Wasm.VI32 val2) -> returnA -< int32 $ if val1 >= val2 then 1 else 0
        (BS32, IGeS, Wasm.VI32 val1, Wasm.VI32 val2) -> returnA -< int32 $ if asInt32 val1 >= asInt32 val2 then 1 else 0

        (BS64, IEq,  Wasm.VI64 val1, Wasm.VI64 val2) -> returnA -< int32 $ if val1 == val2 then 1 else 0
        (BS64, INe,  Wasm.VI64 val1, Wasm.VI64 val2) -> returnA -< int32 $ if val1 /= val2 then 1 else 0
        (BS64, ILtU, Wasm.VI64 val1, Wasm.VI64 val2) -> returnA -< int32 $ if val1 <  val2 then 1 else 0
        (BS64, ILtS, Wasm.VI64 val1, Wasm.VI64 val2) -> returnA -< int32 $ if asInt64 val1 < asInt64 val2 then 1 else 0
        (BS64, IGtU, Wasm.VI64 val1, Wasm.VI64 val2) -> returnA -< int32 $ if val1 >  val2 then 1 else 0
        (BS64, IGtS, Wasm.VI64 val1, Wasm.VI64 val2) -> returnA -< int32 $ if asInt64 val1 > asInt64 val2 then 1 else 0
        (BS64, ILeU, Wasm.VI64 val1, Wasm.VI64 val2) -> returnA -< int32 $ if val1 <= val2 then 1 else 0
        (BS64, ILeS, Wasm.VI64 val1, Wasm.VI64 val2) -> returnA -< int32 $ if asInt64 val1 <= asInt64 val2 then 1 else 0
        (BS64, IGeU, Wasm.VI64 val1, Wasm.VI64 val2) -> returnA -< int32 $ if val1 >= val2 then 1 else 0
        (BS64, IGeS, Wasm.VI64 val1, Wasm.VI64 val2) -> returnA -< int32 $ if asInt64 val1 >= asInt64 val2 then 1 else 0
        _ -> returnA -< error "iRelOp: cannot apply binary operator to given arguments."

    i32eqz = proc (Value v) -> case v of
        (Wasm.VI32 val) -> returnA -< int32 $ if val == 0 then 1 else 0
        _ -> returnA -< error "i32eqz: cannot apply operator to given argument."
    i64eqz = proc (Value v) -> case v of
        (Wasm.VI64 val) -> returnA -< int32 $ if val == 0 then 1 else 0
        _ -> returnA -< error "i64eqz: cannot apply operator to given argument."

    i32ifNeqz f g = proc (v, x) -> case v of
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

instance (ArrowExcept (Exc Value) c) => IsException (Exc Value) Value (ValueT Value c) where
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

-- Conversion functions copied from https://github.com/SPY/haskell-wasm/blob/master/src/Language/Wasm/Interpreter.hs
asInt32 :: Word32 -> Int32
asInt32 w =
    if w < 0x80000000
    then fromIntegral w
    else -1 * fromIntegral (0xFFFFFFFF - w + 1)

asInt64 :: Word64 -> Int64
asInt64 w =
    if w < 0x8000000000000000
    then fromIntegral w
    else -1 * fromIntegral (0xFFFFFFFFFFFFFFFF - w + 1)

asWord32 :: Int32 -> Word32
asWord32 i
    | i >= 0 = fromIntegral i
    | otherwise = 0xFFFFFFFF - fromIntegral (abs i) + 1

asWord64 :: Int64 -> Word64
asWord64 i
    | i >= 0 = fromIntegral i
    | otherwise = 0xFFFFFFFFFFFFFFFF - fromIntegral (abs i) + 1
