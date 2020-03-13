{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC
  -fspecialise-aggressively
  -flate-specialise
  -fsimpl-tick-factor=500
  -fno-warn-orphans
  -fno-warn-partial-type-signatures
#-}
module TypedAnalysis.Parallel where

import           Prelude hiding (not,Bounded,fail,(.),exp,read)

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Environment as Env
import           Control.Arrow.Fix as Fix
import           Control.Arrow.Fix.Cache(ArrowCache,ArrowParallelCache)
import           Control.Arrow.Fix.Stack(ArrowStack)
import           Control.Arrow.Fix.Parallel as Par
import qualified Control.Arrow.Fix.Context as Ctx
import           Control.Arrow.Trans
import           Control.Arrow.Transformer.Value
import           Control.Arrow.Transformer.Abstract.FiniteEnvStore
import           Control.Arrow.Transformer.Abstract.LogError
import           Control.Arrow.Transformer.Abstract.Terminating
import           Control.Arrow.Transformer.Abstract.Fix
import           Control.Arrow.Transformer.Abstract.Fix.Context
import           Control.Arrow.Transformer.Abstract.Fix.Stack as Stack
import           Control.Arrow.Transformer.Abstract.Fix.Cache.Immutable as Cache
import           Control.Arrow.Transformer.Abstract.Fix.Metrics
import           Control.Arrow.Transformer.Abstract.Fix.ControlFlow
-- import           Control.Arrow.Transformer.Abstract.Fix.Trace

import           Control.Monad.State hiding (lift,fail)

import           Data.Label
import           Data.Text (Text)
import qualified Data.HashMap.Lazy as Map
import           Data.HashSet(HashSet)
-- import           Data.Text.Prettyprint.Doc

import qualified Data.Abstract.Widening as W


import           Syntax (Expr(App))
import qualified GenericInterpreter as Generic
import           TypedAnalysis

type InterpParallel x y =
  ValueT Val
    (TerminatingT
      (LogErrorT (HashSet Text)
        (EnvStoreT Text Addr Val
          (FixT
            (MetricsT In
              (StackT Stack.Monotone In
                (CacheT (Parallel Cache.Monotone) In Out
                  (ContextT Ctx
                    (ControlFlowT Expr
                      (->)))))))))) x y

{-# SPECIALIZE Generic.run_ :: InterpParallel [Expr] Val #-}
{-# SPECIALIZE Generic.eval :: InterpParallel [Expr] Val -> InterpParallel Expr Val #-}

eval :: (?sensitivity :: Int)
     => (forall c. (ArrowChoice c, ArrowStack In c, ArrowCache In Out c, ArrowParallelCache In Out c) =>
                   (FixpointCombinator c In Out -> FixpointCombinator c In Out) -> FixpointAlgorithm (c In Out))
     -> [(Text,Addr)] -> [State Label Expr] -> (CFG Expr, (Metrics In, Out))
eval algo env0 e =
  let ?fixpointAlgorithm = algo $ \update ->
        -- Fix.trace printIn printOut .
        -- Fix.traceCache (show . pretty) .
        Ctx.recordCallsite ?sensitivity (\(_,(_,exprs)) -> case exprs of App _ _ l:_ -> Just l; _ -> Nothing) .
        Fix.filter' isFunctionBody update
  in second snd $ run (extend' (Generic.run_ :: InterpParallel [Expr] Val)) W.finite (W.finite, W.finite) (Map.empty,(Map.empty,(env0,e0)))
  where
    e0 = generate (sequence e)
{-# INLINE eval #-}

evalParallel :: Eval
evalParallel = eval Par.parallel

evalADI :: Eval
evalADI = eval Par.adi

evalParallel':: Eval'
evalParallel' exprs = let (metrics,(cfg,res)) = evalParallel [] exprs in (metrics,(cfg,snd res))

evalADI':: Eval'
evalADI' exprs = let (metrics,(cfg,res)) = evalADI [] exprs in (metrics,(cfg,snd res))
