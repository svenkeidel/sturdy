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
import           Control.Arrow.Fix.Chaotic(innermost,outermost)
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
import           Syntax (Expr(App),apply)
import           GenericInterpreter as Generic

type InterpChaotic x y =
  Fix
    (ValueT Val
      (TerminatingT
        (LogErrorT (HashSet Text)
          (EnvStoreT Text Addr Val
            (FixT () ()
              (MetricsT In
                (ComponentT Component In
                  (StackT Stack In
                    (CacheT Monotone In Out
                      (ContextT Ctx
                        (->))))))))))) [Expr] Val x y

{-# SPECIALIZE Generic.run_ :: InterpChaotic [Expr] Val #-}
{-# SPECIALIZE Generic.eval :: InterpChaotic [Expr] Val -> InterpChaotic Expr Val #-}

evalIntervalChaoticInner :: (?sensitivity :: Int) => [(Text,Addr)] -> [State Label Expr] -> (Metrics In, Out)
evalIntervalChaoticInner env0 e = snd $ run (extend' (Generic.run_ :: InterpChaotic [Expr] Val))
    W.finite
    iterationStrategy
    (W.finite, W.finite)
    (Map.empty,(Map.empty,(env0,e0)))
  where
    e0 = generate (sequence e)
    iterationStrategy =
      -- Fix.trace printIn printOut .
      Ctx.recordCallsite ?sensitivity (\(_,(_,exprs)) -> case exprs of [App _ _ l] -> Just l; _ -> Nothing) .
      Fix.filter' apply innermost

evalIntervalChaoticOuter :: (?sensitivity :: Int) => [(Text,Addr)] -> [State Label Expr] -> (Metrics In, Out)
evalIntervalChaoticOuter env0 e = snd $ run (extend' (Generic.run_ :: InterpChaotic [Expr] Val))
    W.finite
    iterationStrategy
    (W.finite, W.finite)
    (Map.empty,(Map.empty,(env0,e0)))
  where
    e0 = generate (sequence e)
    iterationStrategy =
      -- Fix.trace printIn printOut .
      Ctx.recordCallsite ?sensitivity (\(_,(_,exprs)) -> case exprs of [App _ _ l] -> Just l; _ -> Nothing) .
      Fix.filter' apply outermost

evalIntervalChaoticInner' :: (?sensitivity::Int) => [State Label Expr] -> (Metrics In, (HashSet Text, Terminating Val))
evalIntervalChaoticInner' exprs = let (metrics,res) = evalIntervalChaoticInner [] exprs in (metrics,snd res)

evalIntervalChaoticOuter':: (?sensitivity :: Int) => [State Label Expr] -> (Metrics In, (HashSet Text, Terminating Val))
evalIntervalChaoticOuter' exprs = let (metrics,res) = evalIntervalChaoticOuter [] exprs in (metrics,snd res)

evalInterval' :: (?sensitivity :: Int) => [(Text,Addr)] -> [State Label Expr] -> (HashSet Text, Terminating Val)
evalInterval' env exprs = snd $ snd $ evalIntervalChaoticInner env exprs
