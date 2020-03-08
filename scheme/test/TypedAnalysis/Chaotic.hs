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
module TypedAnalysis.Chaotic where

import           Prelude hiding (not,Bounded,fail,(.),exp,read)

import           Control.Category
import           Control.Arrow.Environment as Env
import           Control.Arrow.Fix as Fix
import           Control.Arrow.Fix.Chaotic(IterationStrategy,chaotic,innermost,outermost)
import qualified Control.Arrow.Fix.Context as Ctx
import           Control.Arrow.Trans
import           Control.Arrow.Transformer.Value
import           Control.Arrow.Transformer.Abstract.FiniteEnvStore
import           Control.Arrow.Transformer.Abstract.LogError
import           Control.Arrow.Transformer.Abstract.Fix
import           Control.Arrow.Transformer.Abstract.Fix.Component as Comp
import           Control.Arrow.Transformer.Abstract.Fix.Context
import           Control.Arrow.Transformer.Abstract.Fix.Stack as Stack
import           Control.Arrow.Transformer.Abstract.Fix.Cache.Immutable(CacheT,Monotone)
import           Control.Arrow.Transformer.Abstract.Terminating
import           Control.Arrow.Transformer.Abstract.Fix.Metrics

import           Control.Monad.State hiding (lift,fail)

import           Data.Label
import           Data.Text (Text)
import qualified Data.HashMap.Lazy as Map
import           Data.HashSet(HashSet)

import qualified Data.Abstract.Widening as W
import           Data.Abstract.Terminating(Terminating)

import           TypedAnalysis
import           Syntax (LExpr,Expr(App))
import           GenericInterpreter as Generic

type InterpChaotic x y =
  (ValueT Val
    (TerminatingT
      (LogErrorT (HashSet Text)
        (EnvStoreT Text Addr Val
          (FixT In Out
            (MetricsT In
              (ComponentT Component In
                (StackT Stack In
                  (CacheT Monotone In Out
                    (ContextT Ctx
                      (->))))))))))) x y

{-# SPECIALIZE Generic.run_ :: InterpChaotic [Expr] Val #-}
{-# SPECIALIZE Generic.eval :: InterpChaotic [Expr] Val -> InterpChaotic Expr Val #-}

evalChaotic :: (?sensitivity :: Int) => IterationStrategy _ In Out -> [(Text,Addr)] -> [State Label Expr] -> (Metrics In, Out)
evalChaotic iterationStrat env0 e = snd $ run (extend' (Generic.run_ :: InterpChaotic [Expr] Val))
    W.finite
    algorithm
    (W.finite, W.finite)
    (Map.empty,(Map.empty,(env0,e0)))
  where
    e0 = generate (sequence e)
    algorithm = Fix.fixpointAlgorithm $
      Fix.trace printIn printOut .
      Ctx.recordCallsite ?sensitivity (\(_,(_,exprs)) -> case exprs of App _ _ l:_ -> Just l; _ -> Nothing) .
      Fix.filter' isFunctionBody (chaotic iterationStrat)
{-# INLINE evalChaotic #-}

evalInner :: Eval
evalInner = evalChaotic innermost

evalOuter :: Eval
evalOuter = evalChaotic outermost

evalInner' :: Eval'
evalInner' exprs = let (metrics,res) = evalInner [] exprs in (metrics,snd res)

evalOuter':: Eval'
evalOuter' exprs = let (metrics,res) = evalOuter [] exprs in (metrics,snd res)

eval' :: (?sensitivity :: Int) => [(Text,Addr)] -> [LExpr] -> (HashSet Text,Terminating Val)
eval' env exprs = snd $ snd $ evalInner env exprs
