{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module TaintAnalysis where

import           Prelude as P

import           Abstract
import           Data
import           GenericInterpreter hiding (Exc)
import qualified GenericInterpreter as Generic
import           TaintAnalysisValue hiding (Value)
import qualified TaintAnalysisValue as Taint
import qualified UnitAnalysis as Abs
import           UnitAnalysisValue(Exc(..))
import qualified UnitAnalysisValue as Abs

import           Control.Arrow.Fix as Fix
import           Control.Arrow.Fix.Chaotic (innermost)
import           Control.Arrow.Fix.ControlFlow
import           Control.Arrow.Order
import           Control.Arrow.Trans as Trans

import           Control.Arrow.Transformer.Abstract.Except
import           Control.Arrow.Transformer.Abstract.LogError
import           Control.Arrow.Transformer.Abstract.Fix
import           Control.Arrow.Transformer.Abstract.Fix.Cache.Immutable
import           Control.Arrow.Transformer.Abstract.Fix.Component
import           Control.Arrow.Transformer.Abstract.Fix.ControlFlow
import qualified Control.Arrow.Transformer.Abstract.Fix.Stack as Fix
import           Control.Arrow.Transformer.Abstract.UnitMemory
import           Control.Arrow.Transformer.Abstract.Table
import           Control.Arrow.Transformer.Abstract.Terminating

import           Control.Arrow.Transformer.JumpTypes
import           Control.Arrow.Transformer.Stack (StackT)
import           Control.Arrow.Transformer.StaticGlobalState
import           Control.Arrow.Transformer.Value
import           Control.Arrow.Transformer.WasmFrame

import           Data.Abstract.Except
import           Data.Abstract.MonotoneErrors (Errors)
import           Data.Abstract.Terminating
import qualified Data.Abstract.Widening as W
import           Data.Text.Lazy (Text)
import qualified Data.Vector as Vec

import           Language.Wasm.Interpreter (ModuleInstance)
import qualified Language.Wasm.Interpreter as Wasm
import           Language.Wasm.Structure (ResultType)
import           Language.Wasm.Validate (ValidModule)

import           Numeric.Natural (Natural)

type Value = Taint.Value Abs.Value

type In = (Errors Err,
           (JoinVector Value,
            ((Natural, ModuleInstance),
              (Tables,
              (StaticGlobalState Value,
                (JoinList Value, ([ResultType], [Instruction Natural])))))))

type Out = (Errors Err, Terminating
                (JoinVector Value,
                  (StaticGlobalState Value,
                  Except (Exc Value) (JoinList Value, ()))))


type Result = (CFG (Instruction Natural), (Errors Err,
                                            Terminating
                                             (JoinVector Value,
                                                (StaticGlobalState Value,
                                                 Except (Exc Value) (JoinList Value, [Value])))))

invokeExported :: StaticGlobalState Value
                    -> Tables
                    -> ModuleInstance
                    -> Text
                    -> [Abs.Value]
                    -> Result
invokeExported initialStore tab modInst funcName args =
    let ?cacheWidening = (W.finite,W.finite) in
    let algo = recordControlFlowGraph' getExpression . Fix.filter isRecursive innermost in
    let ?fixpointAlgorithm = fixpointAlgorithm algo in
    (\(cfg,(_,res)) -> (cfg,res)) $ Trans.run
    (Generic.invokeExported ::
      ValueT Value
        (JumpTypesT
          (StackT Value
            (ExceptT (Exc Value)
              (StaticGlobalStateT Value
                (MemoryT
                  (EffectiveAddressT
                    (SizeT Value
                      (SerializeT Value
                        (TableT Value
                          (FrameT FrameData Value
                            (TerminatingT
                              (LogErrorT Err
                                (FixT
                                  (ComponentT Component In
                                    (Fix.StackT Fix.Stack  In
                                      (CacheT Monotone In Out
                                        (ControlFlowT (Instruction Natural)
                                          (->)))))))))))))))))) (Text, [Value]) [Value]) (JoinVector $ Vec.empty,((0,modInst),(tab,(initialStore,([],([],(funcName, P.map taint args)))))))
    where
        taint v = Taint.Value Tainted v
        isRecursive (_,(_,(_,(_,(_,(_,(_,inst))))))) = case inst of
            Loop {} : _ -> True
            Call _ _ : _  -> True
            CallIndirect _ _ : _ -> True
            _           -> False
        -- p1 (locals,(_,(_,(stack,(la, instr))))) = --braces $ hsep (punctuate "," (pretty <$> toList stack))
        --                 hsep [pretty stack, pretty locals, pretty la, pretty instr]
        -- p2 (Terminating (Error.Success (stack, (_,rest)))) = pretty rest
        -- p2 x = pretty x

        getExpression (_,(_,(_,(_,(_,(_,(_,exprs))))))) = case exprs of e:_ -> Just e; _ -> Nothing


instantiateTaint :: ValidModule -> IO (Either String (ModuleInstance, StaticGlobalState Value, Tables))
instantiateTaint valMod = do res <- instantiate valMod (alpha Untainted) (\_ _ -> ()) TableInst
                             return $ fmap (\(m,s,_,tab) -> (m,s,JoinVector tab)) res

alpha :: Taint -> Wasm.Value -> Value
alpha t v = Taint.Value t (Abs.alpha v)

deriving instance ArrowComplete Value c => ArrowComplete Value (ValueT Abs.Value c)
