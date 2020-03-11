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
  -fsimpl-tick-factor=1000
  -fno-warn-orphans
  -fno-warn-partial-type-signatures
#-}
module TypedAnalysis.Parallel where

import           Prelude hiding (not,Bounded,fail,(.),exp,read)

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Fix as Fix
import           Control.Arrow.Fix.Cache (ArrowCache,ArrowParallelCache)
import           Control.Arrow.Fix.Parallel as Par
import           Control.Arrow.Fix.Stack as Stack
import qualified Control.Arrow.Fix.Context as Ctx
import           Control.Arrow.Trans
import           Control.Arrow.Transformer.Value
import           Control.Arrow.Transformer.Abstract.FiniteEnvStore
import           Control.Arrow.Transformer.Abstract.LogError
import           Control.Arrow.Transformer.Abstract.Fix
import           Control.Arrow.Transformer.Abstract.Fix.Context
import           Control.Arrow.Transformer.Abstract.Fix.Stack
import           Control.Arrow.Transformer.Abstract.Fix.Cache.Immutable(CacheT,Parallel,Monotone)
import           Control.Arrow.Transformer.Abstract.Terminating

import           Data.Text (Text)
import qualified Data.HashMap.Lazy as Map
import           Data.HashSet(HashSet)

import qualified Data.Abstract.Widening as W
import           Data.Abstract.Terminating(Terminating)


import           Syntax (Expr(App))
import           GenericInterpreter as Generic
import           TypedAnalysis

type InterpParallel =
  ValueT Val
    (TerminatingT
      (LogErrorT (HashSet Text)
        (EnvStoreT Text Addr Val
          (FixT
            (StackT Stack In
              (CacheT (Parallel Monotone) In Out
                (ContextT Ctx
                  (->))))))))

{-# SPECIALIZE Generic.run_ :: (?fixpointAlgorithm :: FixpointAlgorithm (Fix (InterpParallel [Expr] Val))) => InterpParallel [Expr] Val #-}
{-# SPECIALIZE Generic.eval :: InterpParallel [Expr] Val -> InterpParallel Expr Val #-}

evalParallel :: (?sensitivity :: Int)
             => (forall c. (ArrowChoice c, ArrowStack In c, ArrowCache In Out c, ArrowParallelCache In Out c) =>
                           (FixpointCombinator c In Out -> FixpointCombinator c In Out) -> FixpointAlgorithm (c In Out))
             -> Expr -> (HashSet Text, Terminating Val)
evalParallel algo e =

  let ?fixpointAlgorithm = algo $ \update ->
        Ctx.recordCallsite ?sensitivity (\(_,(_,exprs)) -> case exprs of App _ _ l:_ -> Just l; _ -> Nothing) .
        Fix.filter isFunctionBody update
  in snd $ snd $ run (Generic.run_ :: InterpParallel [Expr] Val) W.finite (W.finite, W.finite)
      (Map.empty,(Map.empty,[e]))

evalPar :: (?sensitivity :: Int) => Expr -> (HashSet Text, Terminating Val)
evalPar = evalParallel Par.parallel

evalADI :: (?sensitivity :: Int) => Expr -> (HashSet Text, Terminating Val)
evalADI = evalParallel Par.adi
