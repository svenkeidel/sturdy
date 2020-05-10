{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
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
module TypedAnalysis.Chaotic where

import           Prelude hiding (not,Bounded,fail,(.),exp,read)

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Environment as Env
import qualified Control.Arrow.Fix as Fix
import           Control.Arrow.Fix.Chaotic(IterationStrategy,chaotic,innermost',outermost')
import qualified Control.Arrow.Fix.Context as Ctx
import qualified Control.Arrow.Fix.ControlFlow as CFlow
import qualified Control.Arrow.Trans as Trans
import           Control.Arrow.Transformer.Value
import           Control.Arrow.Transformer.Abstract.FiniteEnvStore
import           Control.Arrow.Transformer.Abstract.LogError
import           Control.Arrow.Transformer.Abstract.Fix
import           Control.Arrow.Transformer.Abstract.Fix.Component as Comp
import           Control.Arrow.Transformer.Abstract.Fix.Context
import           Control.Arrow.Transformer.Abstract.Fix.Stack as Stack
import           Control.Arrow.Transformer.Abstract.Fix.Cache.Immutable as Cache
import           Control.Arrow.Transformer.Abstract.Fix.Metrics as Metric
import           Control.Arrow.Transformer.Abstract.Fix.ControlFlow
-- import           Control.Arrow.Transformer.Abstract.Fix.Trace
import           Control.Arrow.Transformer.Abstract.Pow

import           Control.Monad.State hiding (lift,fail)

import           Data.Empty
import           Data.Label
import           Data.Text (Text)
import           Data.Abstract.Powerset (Pow)
import qualified Data.Abstract.Powerset as Pow

import qualified Data.Abstract.Widening as W
-- import           Data.Text.Prettyprint.Doc

import           TypedAnalysis
import           Syntax (LExpr,Expr(App))
import           GenericInterpreter as Generic

type InterpChaotic x y =
  (ValueT Val
    (PowT
      (LogErrorT Text
        (EnvStoreT Text Addr Val
          (FixT
            (MetricsT Metric.Monotone In
             (ComponentT Comp.Component  In
               (StackT Stack.Stack In
                 (CacheT Cache.Monotone In Out
                   (ContextT Ctx
                     (ControlFlowT Expr (->)))))))))))) x y

evalChaotic :: (?sensitivity :: Int) => IterationStrategy _ In Out -> [(Text,Addr)] -> [State Label Expr] -> (CFG Expr, (Metric.Monotone In, Out'))
evalChaotic iterationStrat env0 e =
  let ?cacheWidening = (storeErrWidening, W.finite) in
  let ?fixpointAlgorithm = transform $
        Fix.fixpointAlgorithm $
        Fix.trace printIn printOut .
        Ctx.recordCallsite ?sensitivity (\(_,(_,exprs)) -> case Pow.index exprs 0 of App _ _ l:_ -> Just l; _ -> Nothing) .
        Fix.recordEvaluated .
        -- CFlow.recordControlFlowGraph' (\(_,(_,exprs)) -> case exprs of e':_ -> Just e'; _ -> Nothing) .
        -- Fix.filter' isFunctionBody (Fix.trace printIn printOut . chaotic iterationStrat)
        Fix.filter' isFunctionBody (chaotic iterationStrat) in
  second snd $ Trans.run (extend' (Generic.runFixed :: InterpChaotic [Expr] Val)) (empty,(empty,Pow.singleton(env0, e0)))
  where
    e0 = generate (sequence e)
{-# INLINE evalChaotic #-}

evalInner :: Eval
evalInner = evalChaotic innermost'

evalOuter :: Eval
evalOuter = evalChaotic outermost'

evalInner' :: Eval'
evalInner' exprs = let (metrics,(cfg,res)) = evalInner [] exprs in (metrics,(cfg,snd res))

evalOuter':: Eval'
evalOuter' exprs = let (metrics,(cfg,res)) = evalOuter [] exprs in (metrics,(cfg,snd res))

eval' :: (?sensitivity :: Int) => [(Text,Addr)] -> [LExpr] -> (Errors, Pow Val)
eval' env exprs = snd $ snd $ snd $ evalInner env exprs
