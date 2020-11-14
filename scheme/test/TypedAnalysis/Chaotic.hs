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

import           Prelude hiding (not,Bounded,fail,(.),exp,read,IO)

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Environment as Env
import qualified Control.Arrow.Fix as Fix
import           Control.Arrow.Fix.Chaotic(innermost,outermost)
import qualified Control.Arrow.Fix.Context as Ctx
import qualified Control.Arrow.Trans as Trans
import           Control.Arrow.Transformer.Value
import           Control.Arrow.Transformer.Abstract.FiniteEnvStore
import           Control.Arrow.Transformer.Abstract.LogError
import           Control.Arrow.Transformer.Abstract.Fix
import           Control.Arrow.Transformer.Abstract.Fix.Component as Comp
import           Control.Arrow.Transformer.Abstract.Fix.CallSite
import           Control.Arrow.Transformer.Abstract.Fix.Stack as Stack
import           Control.Arrow.Transformer.Abstract.Fix.Cache.Immutable as Cache
import           Control.Arrow.Transformer.Abstract.Fix.Metrics as Metric
import           Control.Arrow.Transformer.Abstract.Fix.ControlFlow
import           Control.Arrow.Transformer.Abstract.Terminating

import           Data.Empty
import           Data.Label
import           Data.Text (Text)

import qualified Data.Abstract.Widening as W
import           Data.Abstract.Terminating(Terminating)

import           TypedAnalysis
import           Syntax (LExpr,Expr(App))
import           GenericInterpreter as Generic

type InterpT c x y =
  (ValueT Val
    (TerminatingT
      (LogErrorT Text
        (EnvStoreT Text Addr Val
          (FixT
            (MetricsT Metric.Monotone In
             (ComponentT Comp.Component  In
               (StackT Stack.Stack In
                 (CacheT Cache.Monotone In Out
                   (CallSiteT Label
                     (ControlFlowT Expr
                       c))))))))))) x y

evalChaotic :: (?sensitivity :: Int) => Fix.FixpointCombinator _ In Out -> [(Text,Addr)] -> [LExpr] -> (CFG Expr, (Metric.Monotone In, Out'))
evalChaotic iterationStrat env0 e =
  let ?fixpointAlgorithm = transform $
        Fix.fixpointAlgorithm $
        -- Fix.trace printIn printOut .
        Fix.filter isApplication (Ctx.recordCallSite ?sensitivity (\(_,(_,exprs)) -> label $ head exprs)) . 
        Fix.recordEvaluated .
        -- CFlow.recordControlFlowGraph' (\(_,(_,exprs)) -> case exprs of e':_ -> Just e'; _ -> Nothing) .
        -- Fix.filter' isFunctionBody (Fix.trace printIn printOut . chaotic iterationStrat)
        Fix.filter' isFunctionBody iterationStrat in
  second snd $ Trans.run (extend' (Generic.runFixed :: InterpT (->) [Expr] Val)) (empty,(empty,(env0,e0)))
  where
    e0 = generate (sequence e)
{-# INLINE evalChaotic #-}

evalInner :: Eval
evalInner =
  let ?cacheWidening = (storeErrWidening, W.finite) in
  evalChaotic innermost

evalOuter :: Eval
evalOuter =
  let ?cacheWidening = (storeErrWidening, W.finite) in
  evalChaotic outermost

evalInner' :: Eval'
evalInner' exprs = let (metrics,(cfg,res)) = evalInner [] exprs in (metrics,(cfg,snd res))

evalOuter':: Eval'
evalOuter' exprs = let (metrics,(cfg,res)) = evalOuter [] exprs in (metrics,(cfg,snd res))

eval' :: (?sensitivity :: Int) => [(Text,Addr)] -> [LExpr] -> (Errors,Terminating Val)
eval' env exprs = snd $ snd $ snd $ evalInner env exprs
