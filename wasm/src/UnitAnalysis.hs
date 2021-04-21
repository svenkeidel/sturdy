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
import           GenericInterpreter hiding (Exc,Err)
import qualified GenericInterpreter as Generic
import           UnitAnalysisValue

import           Control.Arrow
import           Control.Arrow.Fix as Fix
import           Control.Arrow.Fix.Chaotic (innermost)
import           Control.Arrow.Fix.ControlFlow
import           Control.Arrow.Order
import           Control.Arrow.Except (ArrowExcept)
import           Control.Arrow.Trans as Trans

import           Control.Arrow.Transformer.Abstract.Except
import           Control.Arrow.Transformer.Abstract.Error
import           Control.Arrow.Transformer.Abstract.Fix
import           Control.Arrow.Transformer.Abstract.Fix.Cache.Immutable
import           Control.Arrow.Transformer.Abstract.Fix.Component
import           Control.Arrow.Transformer.Abstract.Fix.ControlFlow
import qualified Control.Arrow.Transformer.Abstract.Fix.Stack as Fix
import           Control.Arrow.Transformer.Abstract.Memory
import           Control.Arrow.Transformer.Abstract.Serialize
import           Control.Arrow.Transformer.Abstract.Table
import           Control.Arrow.Transformer.Abstract.Terminating

import           Control.Arrow.Transformer.Stack (StackT)
import           Control.Arrow.Transformer.StaticGlobalState
import           Control.Arrow.Transformer.Reader
import           Control.Arrow.Transformer.Value
import           Control.Arrow.Transformer.WasmFrame


import           Data.Abstract.Terminating
import           Data.Hashable
import           Data.HashSet as HashSet
import           Data.Order
import           Data.Abstract.DiscretePowerset as Pow
import           Data.Abstract.Error as Error
import           Data.Abstract.Except
import qualified Data.Abstract.Widening as W
import           Data.Text.Lazy (Text)
import           Data.Text.Prettyprint.Doc as Pretty
import qualified Data.Vector as Vec

import           Language.Wasm.Interpreter (ModuleInstance)
import qualified Language.Wasm.Interpreter as Wasm
import           Language.Wasm.Validate (ValidModule)

import           Numeric.Natural (Natural)

newtype Exc v = Exc (HashSet (Generic.Exc v)) deriving (Eq, Show, Hashable, PreOrd, Complete)

newtype Err = Err (Pow Generic.Err) deriving (Eq, Show, Hashable, PreOrd, Complete)

instance (Show v) => Pretty (Exc v) where pretty = viaShow
instance (Show n) => Pretty (Instruction n) where pretty = viaShow

alpha :: Wasm.Value -> Value
alpha v = Value $ case v of
  Wasm.VI32 _ -> VI32 ()
  Wasm.VI64 _ -> VI64 ()
  Wasm.VF32 _ -> VF32 ()
  Wasm.VF64 _ -> VF64 ()

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


instance (ArrowExcept (Exc Value) c, ArrowChoice c) => IsException (Exc Value) Value (ValueT Value c) where
    type JoinExc y (ValueT Value c) = ArrowComplete y (ValueT Value c)
    exception = arr $ Exc . HashSet.singleton
    handleException f = proc (Exc excs,x) -> do
                            --ys <- mapList f -< (HashSet.toList excs,x)
                            --joinList _j -< (_init,ys)
                            joinList1'' f -< (HashSet.toList excs,x)

instance Arrow c => IsErr Err (ValueT Value c) where
    err = arr $ Err . Pow.singleton

type In = (JoinVector Value,
            ((Natural, ModuleInstance),
              (Tables,
              (StaticGlobalState Value,
                (JoinList Value, (LabelArities, [Instruction Natural]))))))

type Out = Terminating
             (Error
                Err
                (JoinVector Value,
                --(Tables,
                  (StaticGlobalState Value,
                  Except (Exc Value) (JoinList Value, ()))))


type Result = (CFG (Instruction Natural), Terminating
                                          (Error
                                             Err
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
    let ?cacheWidening = W.finite in
    --let ?fixpointAlgorithm = Function.fix in -- TODO: we need something else here
    --let algo = (trace p1 p2) . (Fix.filter isRecursive $ innermost) in
    let algo = recordControlFlowGraph' getExpression . Fix.filter isRecursive innermost in
    let ?fixpointAlgorithm = fixpointAlgorithm algo in
    (\(cfg,(_,res)) -> (cfg,res)) $ Trans.run
    (Generic.invokeExported ::
      ValueT Value
        (ReaderT Generic.LabelArities
          (StackT Value
            (ExceptT (Exc Value)
              (StaticGlobalStateT Value
                (MemoryT
                  (SerializeT
                    (TableT
                      (FrameT FrameData Value
                        (ErrorT Err
                          (TerminatingT
                            (FixT
                              (ComponentT Component In
                                (Fix.StackT Fix.Stack  In
                                  (CacheT Cache In Out
                                    (ControlFlowT (Instruction Natural)
                                      (->)))))))))))))))) (Text, [Value]) [Value]) (JoinVector $ Vec.empty,((0,modInst),(tab,(initialStore,([],(Generic.LabelArities [],(funcName, args)))))))
    where
        isRecursive (_,(_,(_,(_,(_,(_,inst)))))) = case inst of
            Loop {} : _ -> True
            Call _ _ : _  -> True
            CallIndirect _ _ : _ -> True --error "todo"
            _           -> False
        -- p1 (locals,(_,(_,(stack,(la, instr))))) = --braces $ hsep (punctuate "," (pretty <$> toList stack))
        --                 hsep [pretty stack, pretty locals, pretty la, pretty instr]
        -- p2 (Terminating (Error.Success (stack, (_,rest)))) = pretty rest
        -- p2 x = pretty x

        getExpression (_,(_,(_,(_,(_,(_,exprs)))))) = case exprs of e:_ -> Just e; _ -> Nothing

instantiateAbstract :: ValidModule -> IO (Either String (ModuleInstance, StaticGlobalState Value, Tables))
instantiateAbstract valMod = do res <- instantiate valMod alpha (\_ _ -> ()) TableInst
                                return $ fmap (\(m,s,_,tab) -> (m,s,JoinVector tab)) res
