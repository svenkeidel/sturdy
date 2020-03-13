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
  -funfolding-use-threshold=1000
  -flate-dmd-anal
  -fsimpl-tick-factor=50000
  -fmax-simplifier-iterations=10
  -fexpose-all-unfoldings
  -fno-warn-orphans
  -fno-warn-partial-type-signatures
#-}
-- Don't use these!!!!
-- {-# OPTIONS_GHC
--   -funfolding-dict-discount=200
--   -funfolding-fun-discount=200
--   -funfolding-keeness-factor=3
--   -funfolding-creation-threshold=10000
-- #-}
module TypedAnalysis.Chaotic where

import           Prelude hiding (not,Bounded,fail,(.),exp,read)

import           Control.Category
import           Control.Arrow.Fix (FixpointAlgorithm,Fix)
import qualified Control.Arrow.Fix as Fix
import           Control.Arrow.Fix.Chaotic(IterationStrategy,chaotic,innermost,outermost)
import qualified Control.Arrow.Fix.Context as Ctx
import           Control.Arrow.Order(ArrowComplete)
import           Control.Arrow.Trans
import           Control.Arrow.Transformer.Value
import           Control.Arrow.Transformer.NoInline
import           Control.Arrow.Transformer.Abstract.FiniteEnvStore
import           Control.Arrow.Transformer.Abstract.LogError
import           Control.Arrow.Transformer.Abstract.Fix
import           Control.Arrow.Transformer.Abstract.Fix.Component as Comp
import           Control.Arrow.Transformer.Abstract.Fix.Context
import           Control.Arrow.Transformer.Abstract.Fix.Stack as Stack
import           Control.Arrow.Transformer.Abstract.Fix.Cache.Immutable as Cache
import           Control.Arrow.Transformer.Abstract.Terminating

import           Data.Text (Text)
import qualified Data.HashMap.Lazy as Map
import           Data.HashSet(HashSet)

import qualified Data.Abstract.Widening as W
import           Data.Abstract.Terminating(Terminating)
import           Data.Order

import           TypedAnalysis
import           Syntax (Expr(App))
import           GenericInterpreter as Generic

type InterpChaotic =
  ValueT Val
    (TerminatingT
      (LogErrorT Text
        (EnvStoreT Text Addr Val
          (FixT
            (ComponentT Comp.Monotone In
              (StackT Stack.Monotone In
                (CacheT Cache.Monotone In Out
                  (ContextT Ctx
                    (->)))))))))

type InterpChaoticFix = Fix (InterpChaotic [Expr] Val)

{-# SPECIALIZE if__ :: (ArrowComplete z InterpChaotic)
                    => InterpChaotic x z -> InterpChaotic y z -> InterpChaotic (Val,(x,y)) z #-}
{-# SPECIALIZE Generic.run_ :: (?fixpointAlgorithm :: FixpointAlgorithm (Fix (InterpChaotic [Expr] Val)))
                            => InterpChaotic [Expr] Val #-}
-- {-# SPECIALIZE Generic.eval :: InterpChaotic [Expr] Val -> InterpChaotic Expr Val #-}

-- evalChaotic :: (?sensitivity :: Int) => IterationStrategy _ In Out -> Expr -> (HashSet Text, Terminating Val)
-- evalChaotic iterationStrat e =
--   let ?fixpointAlgorithm = fixpointAlgorithm iterationStrat
--   in snd $ snd $ run (Generic.run_ :: InterpChaotic [Expr] Val) (Map.empty,(Map.empty,[e]))
-- {-# INLINE evalChaotic #-}


-- fixpointAlgorithm :: (?sensitivity :: Int) => IterationStrategy _ In Out -> FixpointAlgorithm InterpChaoticFix
-- fixpointAlgorithm iterationStrat =
--   let ?cacheWidening = (W.finite, W.finite) in
--   Fix.fixpointAlgorithm $
--     -- Fix.trace printInExpr printOutVal .
--     Ctx.recordCallsite ?sensitivity (\(_,(_,exprs)) -> case exprs of App _ _ l:_ -> Just l; _ -> Nothing) .
--     Fix.filter isFunctionBody (chaotic iterationStrat)

-- evalInner :: (?sensitivity :: Int) => Expr -> (HashSet Text, Terminating Val)
-- evalInner = evalChaotic innermost

-- evalOuter :: (?sensitivity :: Int) => Expr -> (HashSet Text, Terminating Val)
-- evalOuter = evalChaotic outermost
