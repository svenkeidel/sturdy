{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module ConcreteInterpreter where

import           Frame
import           GenericInterpreter hiding (eval,evalNumericInst,evalVariableInstr,evalParametricInst)
import qualified GenericInterpreter as Generic
--import           Stack

import           Control.Arrow
import           Control.Arrow.Const
import           Control.Arrow.Except
import           Control.Arrow.Fail
import           Control.Arrow.Stack
import           Control.Arrow.State
import           Control.Arrow.Store
import qualified Control.Arrow.Trans as Trans
import           Control.Arrow.Transformer.Kleisli
import           Control.Arrow.Transformer.Stack
import           Control.Arrow.Trans
import           Control.Arrow.Transformer.Value

import           Control.Arrow.Transformer.Concrete.Except
import           Control.Arrow.Transformer.State

import           Control.Category

import           Data.Concrete.Error

import qualified Data.Function as Function
import           Data.Profunctor
import           Data.Vector (Vector, (!), (//))
import qualified Data.Vector as Vec

import           Language.Wasm.Interpreter hiding (Value)
import qualified Language.Wasm.Interpreter as Wasm
import           Language.Wasm.Structure hiding (exports)

import           Numeric.Natural (Natural)

data WasmStore v c = WasmStore {
    funcInstances :: Vector (FuncInst v c), 
    tableInstances :: Vector TableInst,
    memInstances :: Vector MemInst,
    globalInstances :: Vector v
} deriving (Show)

emptyWasmStore :: WasmStore v c
emptyWasmStore = WasmStore {
    funcInstances = Vec.empty,
    tableInstances = Vec.empty,
    memInstances = Vec.empty,
    globalInstances = Vec.empty
}

data FuncInst v c =
    FuncInst {
        funcType :: FuncType,
        moduleInstance :: ModuleInstance,
        code :: Function
    }
    | HostInst {
        funcType :: FuncType,
        hostCode :: HostFunction v c
    } deriving (Show)

data TableInst = TableInst deriving (Show)
data MemInst = MemInst deriving (Show)

newtype WasmStoreT v c x y = WasmStoreT (StateT (WasmStore v c) c x y)
    deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowLift,
              ArrowFail e, ArrowExcept e, ArrowConst r, ArrowStore var' val', ArrowRun, ArrowFrame fd val,
              ArrowStack st, ArrowState (WasmStore v c))


--instance ArrowState (WasmStore v cHost) (WasmStoreT v cHost c) where
--    get = lift' get
--    put = lift' put

instance ArrowTrans (WasmStoreT v) where
    -- lift' :: c x y -> WasmStoreT v c x y
    lift' arr = WasmStoreT (lift' arr)

instance (ArrowChoice c, Profunctor c) => ArrowWasmStore v (WasmStoreT v c) where
    readGlobal = 
        proc i -> do
            WasmStore{globalInstances=vec} <- get -< ()
            returnA -< vec ! i
    writeGlobal =
        proc (i,v) -> do
            store@WasmStore{globalInstances=vec} <- get -< ()
            put -< store{globalInstances=vec // [(i, v)]}
    
    readFunction funcCont hostCont = proc (i,x) -> do
        WasmStore{funcInstances = fs} <- get -< ()
        case fs ! i of
            FuncInst fTy modInst code -> funcCont -< ((fTy,modInst,code),x)
            HostInst fTy code         -> returnA -< error "" --hostCont -< ((fTy,code),x)

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


type TransStack = FrameT FrameData Value (StackT Value (->))

evalVariableInst :: (Instruction Natural) -> [Value] -> FrameData -> Vector Value 
            -> WasmStore Value TransStack -> ([Value], (Vector Value, (WasmStore Value TransStack, ())))
evalVariableInst inst stack fd locals store =
    Trans.run
      (Generic.evalVariableInst ::
        WasmStoreT Value
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

--eval inst =
--    let ?fixpointAlgorithm = Function.fix in
--    Trans.run
--    (Generic.eval ::
--      ValueT Value
--        (ExceptT _ --(Generic.Exc Value)
--          (StackT Value 
--            (FrameT FrameData Value
--              (->)))) [Instruction Natural] ()) inst
