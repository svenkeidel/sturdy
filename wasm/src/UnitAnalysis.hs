{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module UnitAnalysis where

import           Abstract
import           Data
import           GenericInterpreter hiding (Exc)
import qualified GenericInterpreter as Generic
import           UnitAnalysisValue

import           Control.Arrow
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
import           Control.Arrow.Transformer.Abstract.Table
import           Control.Arrow.Transformer.Abstract.Terminating
import           Control.Arrow.Transformer.Abstract.UnitMemory

import           Control.Arrow.Transformer.JumpTypes
import           Control.Arrow.Transformer.Stack (StackT)
import           Control.Arrow.Transformer.StaticGlobalState
import           Control.Arrow.Transformer.Value
import           Control.Arrow.Transformer.WasmFrame


import           Data.Abstract.Terminating
import           Data.Abstract.Except
import           Data.Abstract.MonotoneErrors (Errors)
import qualified Data.Abstract.Widening as W
import           Data.Text.Lazy (Text)
import qualified Data.Vector as Vec

import           Language.Wasm.Interpreter (ModuleInstance)
import           Language.Wasm.Structure (ResultType)
import           Language.Wasm.Validate (ValidModule)

import           Numeric.Natural (Natural)

mapList :: (ArrowChoice c) => c (a,x) y -> c ([a],x) [y]
mapList f = proc (as,x) -> do
  case as of
    [] -> returnA -< []
    (a:ass) -> do
      bs <- mapList f -< (ass,x)
      b <- f -< (a,x)
      returnA -< b:bs

tailA :: (ArrowChoice c) => c () [a] -> c () [a]
tailA f = proc () -> do
  aList <- f -< ()
  case aList of
    (_:as) -> returnA -< as
    []     -> returnA -< error "tailA: cannot return the tail of an empty list"


type In = (Errors Err,
           (JoinVector Value,
            ((Natural, ModuleInstance),
              (Tables,
              (StaticGlobalState Value,
                (JoinList Value, ([ResultType], [Instruction Natural])))))))

type Out = (Errors Err, (Terminating
                (JoinVector Value,
                --(Tables,
                  (StaticGlobalState Value,
                  Except (Exc Value) (JoinList Value, ())))))


type Result = (CFG (Instruction Natural), (Errors Err,
                                            Terminating
                                             (JoinVector Value,
                                              --(Tables,
                                                (StaticGlobalState Value,
                                                 Except (Exc Value) (JoinList Value, [Value])))))

invokeExported :: StaticGlobalState Value
                                     -> Tables
                                     -> ModuleInstance
                                     -> Text
                                     -> [Value]
                                     -> Result
invokeExported initialStore tab modInst funcName args =
    let ?cacheWidening = (W.finite,W.finite) in
    --let ?fixpointAlgorithm = Function.fix in -- TODO: we need something else here
    --let algo = (trace p1 p2) . (Fix.filter isRecursive $ innermost) in
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
                                    (Fix.StackT Fix.Stack In
                                      (CacheT Monotone In Out
                                        (ControlFlowT (Instruction Natural)
                                          (->)))))))))))))))))) (Text, [Value]) [Value]) (JoinVector $ Vec.empty,((0,modInst),(tab,(initialStore,([],([],(funcName, args)))))))
    where
        isRecursive (_,(_,(_,(_,(_,(_,(_,inst))))))) = case inst of
            Loop {} : _ -> True
            Call _ _ : _  -> True
            CallIndirect _ _ : _ -> True --error "todo"
            _           -> False
        -- p1 (locals,(_,(_,(stack,(la, instr))))) = --braces $ hsep (punctuate "," (pretty <$> toList stack))
        --                 hsep [pretty stack, pretty locals, pretty la, pretty instr]
        -- p2 (Terminating (Error.Success (stack, (_,rest)))) = pretty rest
        -- p2 x = pretty x

        getExpression (_,(_,(_,(_,(_,(_,(_,exprs))))))) = case exprs of e:_ -> Just e; _ -> Nothing

instantiateAbstract :: ValidModule -> IO (Either String (ModuleInstance, StaticGlobalState Value, Tables))
instantiateAbstract valMod = do res <- instantiate valMod alpha (\_ _ -> ()) TableInst
                                return $ fmap (\(m,s,_,tab) -> (m,s,JoinVector tab)) res

deriving instance ArrowComplete y c => ArrowComplete y (ValueT Value c)