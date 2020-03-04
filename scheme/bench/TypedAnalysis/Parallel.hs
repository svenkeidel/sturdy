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
import           Control.Arrow.Environment as Env
import           Control.Arrow.Fix as Fix
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

import           Control.Monad.State hiding (lift,fail)

import           Data.Label
import           Data.Text (Text)
import qualified Data.HashMap.Lazy as Map
import           Data.HashSet(HashSet)

import qualified Data.Abstract.Widening as W
import           Data.Abstract.Terminating(Terminating)


import           Syntax (Expr(App),apply, nonEmpty)
import           GenericInterpreter as Generic
import           TypedAnalysis

type InterpParallel x y =
  Fix
    (ValueT Val
      (TerminatingT
        (LogErrorT (HashSet Text)
          (EnvStoreT Text Addr Val
            (FixT () ()
              (StackT Stack In
                (CacheT (Parallel Monotone) In Out
                  (ContextT Ctx
                    (->))))))))) [Expr] Val x y

{-# SPECIALIZE Generic.run_ :: InterpParallel [Expr] Val #-}
{-# SPECIALIZE Generic.eval :: InterpParallel [Expr] Val -> InterpParallel Expr Val #-}

evalParallel :: (?sensitivity :: Int) => Expr -> (HashSet Text, Terminating Val)
evalParallel e = snd $ snd $ run (Generic.run_ :: InterpParallel [Expr] Val)
    W.finite
    iterationStrategy
    (W.finite, W.finite)
    (Map.empty,(Map.empty,[e]))
  where
    iterationStrategy =
      -- Fix.trace printIn printOut .
      Ctx.recordCallsite ?sensitivity (\(_,(_,exprs)) -> case exprs of [App _ _ l] -> Just l; _ -> Nothing) .
      Stack.topLevel (Fix.filter nonEmpty (Par.iterate . Par.update))
                     (Fix.filter apply Par.update)

evalParallelADI :: (?sensitivity :: Int) => Expr -> (HashSet Text, Terminating Val)
evalParallelADI e = snd $ snd $ run (Generic.run_ :: InterpParallel [Expr] Val)
    W.finite
    iterationStrategy
    (W.finite, W.finite)
    (Map.empty,(Map.empty,[e]))
  where
    iterationStrategy =
      -- Fix.trace printIn printOut .
      Ctx.recordCallsite ?sensitivity (\(_,(_,exprs)) -> case exprs of [App _ _ l] -> Just l; _ -> Nothing) .
      Stack.topLevel (Fix.filter nonEmpty (Par.iterate . Par.updateADI))
                     (Fix.filter apply Par.updateADI)
