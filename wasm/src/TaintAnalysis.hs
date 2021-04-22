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

import           GenericInterpreter hiding (Exc,Err)
import qualified GenericInterpreter as Generic
import           UnitAnalysisValue(Exc(..),Err)

import           Abstract
import           Data
import           TaintAnalysisValue
import qualified UnitAnalysisValue as Abs

import           Control.Arrow.Fix as Fix
import           Control.Arrow.Fix.Chaotic (innermost)
import           Control.Arrow.Fix.ControlFlow
import           Control.Arrow.Order
import           Control.Arrow.Trans as Trans

import           Control.Arrow.Transformer.Abstract.Error
import           Control.Arrow.Transformer.Abstract.Except
import           Control.Arrow.Transformer.Abstract.Fix
import           Control.Arrow.Transformer.Abstract.Fix.Cache.Immutable
import           Control.Arrow.Transformer.Abstract.Fix.Component
import           Control.Arrow.Transformer.Abstract.Fix.ControlFlow
import qualified Control.Arrow.Transformer.Abstract.Fix.Stack as Fix
import           Control.Arrow.Transformer.Abstract.Memory
import           Control.Arrow.Transformer.Abstract.Serialize
import           Control.Arrow.Transformer.Abstract.UnitMemAddress
import           Control.Arrow.Transformer.Abstract.UnitSize
import           Control.Arrow.Transformer.Abstract.Table
import           Control.Arrow.Transformer.Abstract.Terminating

import           Control.Arrow.Transformer.Reader
import           Control.Arrow.Transformer.Stack (StackT)
import           Control.Arrow.Transformer.StaticGlobalState
import           Control.Arrow.Transformer.Value
import           Control.Arrow.Transformer.WasmFrame

import           Data.Abstract.Error as Error
import           Data.Abstract.Except
import           Data.Abstract.Terminating
import qualified Data.Abstract.Widening as W
import           Data.Text.Lazy (Text)
import qualified Data.Vector as Vec

import           Language.Wasm.Interpreter (ModuleInstance)

import           Numeric.Natural (Natural)

type In = (JoinVector (Value Abs.Value),
            ((Natural, ModuleInstance),
              (Tables,
              (StaticGlobalState (Value Abs.Value),
                (JoinList (Value Abs.Value), (JumpTypes, [Instruction Natural]))))))

type Out = Terminating
             (Error
                Err
                (JoinVector (Value Abs.Value),
                  (StaticGlobalState (Value Abs.Value),
                  Except (Exc (Value Abs.Value)) (JoinList (Value Abs.Value), ()))))


type Result = (CFG (Instruction Natural), Terminating
                                          (Error
                                             Err
                                             (JoinVector (Value Abs.Value),
                                                (StaticGlobalState (Value Abs.Value),
                                                 Except (Exc (Value Abs.Value)) (JoinList (Value Abs.Value), [Value Abs.Value])))))

invokeExported :: StaticGlobalState (Value Abs.Value)
                    -> Tables
                    -> ModuleInstance
                    -> Text
                    -> [Abs.Value]
                    -> Result
invokeExported initialStore tab modInst funcName args =
    let ?cacheWidening = W.finite in
    --let ?fixpointAlgorithm = Function.fix in -- TODO: we need something else here
    --let algo = (trace p1 p2) . (Fix.filter isRecursive $ innermost) in
    let algo = recordControlFlowGraph' getExpression . Fix.filter isRecursive innermost in
    let ?fixpointAlgorithm = fixpointAlgorithm algo in
    (\(cfg,(_,res)) -> (cfg,res)) $ Trans.run
    (Generic.invokeExported ::
      ValueT (Value Abs.Value)
        (ReaderT Generic.JumpTypes
          (StackT (Value Abs.Value)
            (ExceptT (Exc (Value Abs.Value))
              (StaticGlobalStateT (Value Abs.Value)
                (MemoryT
                  (MemAddressT
                    (SizeT (Value Abs.Value)
                      (SerializeT (Value Abs.Value)
                        (TableT (Value Abs.Value)
                          (FrameT FrameData (Value Abs.Value)
                            (ErrorT Err
                              (TerminatingT
                                (FixT
                                  (ComponentT Component In
                                    (Fix.StackT Fix.Stack  In
                                      (CacheT Cache In Out
                                        (ControlFlowT (Instruction Natural)
                                          (->)))))))))))))))))) (Text, [Value Abs.Value]) [Value Abs.Value]) (JoinVector $ Vec.empty,((0,modInst),(tab,(initialStore,([],(Generic.JumpTypes [],(funcName, P.map taint args)))))))
    where
        taint v = Value Tainted v
        isRecursive (_,(_,(_,(_,(_,(_,inst)))))) = case inst of
            Loop {} : _ -> True
            Call _ _ : _  -> True
            CallIndirect _ _ : _ -> True
            _           -> False
        -- p1 (locals,(_,(_,(stack,(la, instr))))) = --braces $ hsep (punctuate "," (pretty <$> toList stack))
        --                 hsep [pretty stack, pretty locals, pretty la, pretty instr]
        -- p2 (Terminating (Error.Success (stack, (_,rest)))) = pretty rest
        -- p2 x = pretty x

        getExpression (_,(_,(_,(_,(_,(_,exprs)))))) = case exprs of e:_ -> Just e; _ -> Nothing


deriving instance ArrowComplete (Value Abs.Value) c => ArrowComplete (Value Abs.Value) (ValueT Abs.Value c)
