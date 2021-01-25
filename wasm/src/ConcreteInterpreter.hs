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
import           GenericInterpreter hiding (eval,evalNumericInst,evalVariableInstr)
import qualified GenericInterpreter as Generic
--import           Stack

import           Control.Arrow
import           Control.Arrow.Except
import qualified Control.Arrow.Trans as Trans
import           Control.Arrow.Transformer.Kleisli
import           Control.Arrow.Transformer.Stack
import           Control.Arrow.Transformer.Value

import           Control.Arrow.Transformer.Concrete.Except

import           Data.Concrete.Error

import qualified Data.Function as Function
import           Data.Vector (Vector)

import           Language.Wasm.Interpreter hiding (Value)
import qualified Language.Wasm.Interpreter as Wasm
import           Language.Wasm.Structure hiding (exports)

import           Numeric.Natural (Natural)

newtype Value = Value Wasm.Value deriving (Show, Eq)

instance (ArrowChoice c) => IsVal Value (ValueT Value c) where
    i32const = proc w32 -> returnA -< Value $ VI32 w32   
    i64const = proc w64 -> returnA -< Value $ VI64 w64
    iBinOp = proc (bs,op,Value v1,Value v2) ->
                case bs of 
                    BS32 -> do
                                case op of
                                    IAdd -> returnA -< Just $ Value $ addVal v1 v2

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


evalVariableInst :: (Instruction Natural) -> [Value] -> FrameData -> Vector Value 
            -> Vector Value -> ([Value], (Vector Value, (Vector Value, ())))
evalVariableInst inst stack fd locals globals =
    Trans.run
      (Generic.evalVariableInst ::
        WasmStoreT Value
          (FrameT FrameData Value
            (StackT Value
              (->))) (Instruction Natural) ()) (stack, (locals, (fd,(globals, inst))))

--eval inst =
--    let ?fixpointAlgorithm = Function.fix in
--    Trans.run
--    (Generic.eval ::
--      ValueT Value
--        (ExceptT _ --(Generic.Exc Value)
--          (StackT Value 
--            (FrameT FrameData Value
--              (->)))) [Instruction Natural] ()) inst
