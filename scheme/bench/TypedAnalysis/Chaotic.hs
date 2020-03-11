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
  -funfolding-use-threshold=3000
  -fsimpl-tick-factor=1000
  -fno-warn-orphans
  -fno-warn-partial-type-signatures
#-}
module TypedAnalysis.Chaotic where

import           Prelude hiding (not,Bounded,fail,(.),exp,read)

import           Control.Category
import           Control.Arrow.Fix as Fix
import           Control.Arrow.Fix.Chaotic(IterationStrategy,chaotic,innermost,outermost)
import qualified Control.Arrow.Fix.Context as Ctx
import           Control.Arrow.Trans
import           Control.Arrow.Transformer.Value
import           Control.Arrow.Transformer.Abstract.FiniteEnvStore
import           Control.Arrow.Transformer.Abstract.LogError
import           Control.Arrow.Transformer.Abstract.Fix
import           Control.Arrow.Transformer.Abstract.Fix.Component
import           Control.Arrow.Transformer.Abstract.Fix.Context
import           Control.Arrow.Transformer.Abstract.Fix.Stack
import           Control.Arrow.Transformer.Abstract.Fix.Cache.Immutable(CacheT,Monotone)
import           Control.Arrow.Transformer.Abstract.Terminating

import           Data.Text (Text)
import qualified Data.HashMap.Lazy as Map
import           Data.HashSet(HashSet)

import qualified Data.Abstract.Widening as W
import           Data.Abstract.Terminating(Terminating)

import           TypedAnalysis
import           Syntax (Expr(App))
import           GenericInterpreter as Generic

type InterpChaotic =
  ValueT Val
    (TerminatingT
      (LogErrorT (HashSet Text)
        (EnvStoreT Text Addr Val
          (FixT
            (ComponentT Component In
              (StackT Stack In
                (CacheT Monotone In Out
                  (ContextT Ctx
                    (->)))))))))

{-# SPECIALIZE Generic.run_ :: (?fixpointAlgorithm :: FixpointAlgorithm (Fix (InterpChaotic [Expr] Val)))
                            => InterpChaotic [Expr] Val #-}
{-# SPECIALIZE Generic.eval :: InterpChaotic [Expr] Val -> InterpChaotic Expr Val #-}

evalChaotic :: (?sensitivity :: Int) => IterationStrategy _ In Out -> Expr -> (HashSet Text, Terminating Val)
evalChaotic iterationStrat e =
  let ?fixpointAlgorithm =
        {-# SCC "fixpointAlgorithm" #-}
        Fix.fixpointAlgorithm $
          -- Fix.trace printIn printOut .
          Ctx.recordCallsite ?sensitivity (\(_,(_,exprs)) -> case exprs of App _ _ l:_ -> Just l; _ -> Nothing) .
          Fix.filter isFunctionBody (chaotic iterationStrat)
  in snd $ snd $ run (Generic.run_ :: InterpChaotic [Expr] Val)
       W.finite ({-# SCC "wideningStore" #-} W.finite, W.finite)
       (Map.empty,(Map.empty,[e]))
{-# INLINE evalChaotic #-}

evalInner :: (?sensitivity :: Int) => Expr -> (HashSet Text, Terminating Val)
evalInner = evalChaotic innermost

evalOuter :: (?sensitivity :: Int) => Expr -> (HashSet Text, Terminating Val)
evalOuter = evalChaotic outermost
