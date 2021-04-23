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
import           GenericInterpreter hiding (Exc,Err)
import qualified GenericInterpreter as Generic
import           TaintAnalysisValue hiding (Value)
import qualified TaintAnalysisValue as Taint
import qualified UnitAnalysis as Abs
import           UnitAnalysisValue(Exc(..),Err)
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
import qualified Language.Wasm.Interpreter as Wasm
import           Language.Wasm.Validate (ValidModule)

import           Numeric.Natural (Natural)

type Value = Taint.Value Abs.Value

type In = (JoinVector Value,
            ((Natural, ModuleInstance),
              (Tables,
              (StaticGlobalState Value,
                (JoinList Value, (JumpTypes, [Instruction Natural]))))))

type Out = Terminating
             (Error
                Err
                (JoinVector Value,
                  (StaticGlobalState Value,
                  Except (Exc Value) (JoinList Value, ()))))


type Result = (CFG (Instruction Natural), Terminating
                                          (Error
                                             Err
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
    let ?cacheWidening = W.finite in
    --let ?fixpointAlgorithm = Function.fix in -- TODO: we need something else here
    --let algo = (trace p1 p2) . (Fix.filter isRecursive $ innermost) in
    let algo = recordControlFlowGraph' getExpression . Fix.filter isRecursive innermost in
    let ?fixpointAlgorithm = fixpointAlgorithm algo in
    (\(cfg,(_,res)) -> (cfg,res)) $ Trans.run
    (Generic.invokeExported ::
      ValueT Value
        (ReaderT Generic.JumpTypes
          (StackT Value
            (ExceptT (Exc Value)
              (StaticGlobalStateT Value
                (MemoryT
                  (MemAddressT
                    (SizeT Value
                      (SerializeT Value
                        (TableT Value
                          (FrameT FrameData Value
                            (ErrorT Err
                              (TerminatingT
                                (FixT
                                  (ComponentT Component In
                                    (Fix.StackT Fix.Stack  In
                                      (CacheT Cache In Out
                                        (ControlFlowT (Instruction Natural)
                                          (->)))))))))))))))))) (Text, [Value]) [Value]) (JoinVector $ Vec.empty,((0,modInst),(tab,(initialStore,([],(Generic.JumpTypes [],(funcName, P.map taint args)))))))
    where
        taint v = Taint.Value Tainted v
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


instantiateTaint :: ValidModule -> IO (Either String (ModuleInstance, StaticGlobalState Value, Tables))
instantiateTaint valMod = do res <- instantiate valMod (alpha Untainted) (\_ _ -> ()) TableInst
                             return $ fmap (\(m,s,_,tab) -> (m,s,JoinVector tab)) res

alpha :: Taint -> Wasm.Value -> Value
alpha t v = Taint.Value t (Abs.alpha v)

deriving instance ArrowComplete Value c => ArrowComplete Value (ValueT Abs.Value c)
