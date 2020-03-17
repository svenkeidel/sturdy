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
  -flate-dmd-anal
  -funfolding-use-threshold=5000
  -fexpose-all-unfoldings
  -fsimpl-tick-factor=50000
  -fmax-simplifier-iterations=10
  -fno-warn-orphans
  -fno-warn-partial-type-signatures
#-}
{-
 Don't use these!!!!
  -funfolding-creation-threshold=100000
  -funfolding-dict-discount=1000
  -funfolding-fun-discount=1000
  -funfolding-keeness-factor=30
-}
module TypedAnalysis.Chaotic where

import           Prelude hiding (not,Bounded,fail,(.),exp,read)

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Fix (FixpointAlgorithm,FixpointCombinator,Fix)
import qualified Control.Arrow.Fix as Fix
import           Control.Arrow.Fix.Stack
import           Control.Arrow.Fix.Cache
import           Control.Arrow.Fix.Chaotic
import qualified Control.Arrow.Fix.Context as Ctx
import           Control.Arrow.Order(ArrowComplete)
import qualified Control.Arrow.Trans as Trans
import           Control.Arrow.Transformer.Value
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
import           Data.Profunctor

import qualified Data.Abstract.Widening as W
import           Data.Abstract.Terminating(Terminating)

import           Syntax (Expr(App))
import qualified GenericInterpreter as Generic
import           TypedAnalysis

import           GHC.Exts

type Interp =
  ValueT Val
    (TerminatingT
      (LogErrorT Text
        (EnvStoreT Text Addr Val
          (FixT
            (ComponentT Comp.Component In
              (StackT Stack.Monotone In
                (CacheT Cache.Monotone In Out
                  (ContextT Ctx
                    (->)))))))))

{-# SPECIALIZE if__ :: (ArrowComplete z Interp)
                    => Interp x z -> Interp y z -> Interp (Val,(x,y)) z #-}
{-# SPECIALIZE Generic.eval :: Interp [Expr] Val -> Interp Expr Val #-}
{-# SPECIALIZE Generic.run :: Interp Expr Val -> Interp [Expr] Val -> Interp [Expr] Val #-}
{-# SPECIALIZE Generic.runFixed :: FixpointAlgorithm (Fix (Interp [Expr] Val)) -> Interp [Expr] Val #-}

evalInner :: (?sensitivity :: Int) => Expr -> (HashSet Text, Terminating Val)
evalInner e =
  snd $ snd $ Trans.run (Generic.runFixed algorithm :: Interp [Expr] Val) (Map.empty,(Map.empty,[e]))
  where
    algorithm :: FixpointAlgorithm (Fix (Interp [Expr] Val))
    algorithm =
      let ?cacheWidening = (W.finite, W.finite) in
      transform $ inline Fix.fixpointAlgorithm $
        -- Fix.trace printInExpr printOutVal .
        Ctx.recordCallsite ?sensitivity (\(_,(_,exprs)) -> case exprs of App _ _ l:_ -> Just l; _ -> Nothing) .
        Fix.filter isFunctionBody (chaotic innermost)

evalOuter :: (?sensitivity :: Int) => Expr -> (HashSet Text, Terminating Val)
evalOuter e =
  snd $ snd $ Trans.run (Generic.runFixed algorithm :: Interp [Expr] Val) (Map.empty,(Map.empty,[e]))
  where
    algorithm :: FixpointAlgorithm (Fix (Interp [Expr] Val))
    algorithm =
      let ?cacheWidening = (W.finite, W.finite) in
      transform $ inline Fix.fixpointAlgorithm $
        -- Fix.trace printInExpr printOutVal .
        Ctx.recordCallsite ?sensitivity (\(_,(_,exprs)) -> case exprs of App _ _ l:_ -> Just l; _ -> Nothing) .
        Fix.filter isFunctionBody (chaotic outermost)

-- type InterpFix = (FixT
--             (ComponentT Comp.Monotone In
--               (StackT Stack.Monotone In
--                 (CacheT Cache.Monotone In Out
--                   (ContextT Ctx
--                     (->))))))
-- inner :: (?sensitivity :: Int, ?cacheWidening :: Widening c,
--           ArrowChoice c, Ctx.ArrowContext Ctx c, ArrowStack In c,
--           ArrowComponent In c, ArrowInComponent In c, ArrowCache In Out c)
--       => FixpointCombinator c In Out
-- inner x =
--   -- Fix.trace printInExpr printOutVal .
--   Ctx.recordCallsite ?sensitivity (\(_,(_,exprs)) -> case exprs of App _ _ l:_ -> Just l; _ -> Nothing) $
--   Fix.filter isFunctionBody chaoticInner $
--   x

-- {-# SPECIALIZE inner :: (?sensitivity :: Int,
--                          ?cacheWidening :: (W.Widening (Store,Errors), W.Widening (Terminating Val)))
--                      => InterpFix In Out -> InterpFix In Out #-}

-- chaoticInner :: (?cacheWidening :: Widening c,
--           ArrowChoice c, ArrowStack In c,
--           ArrowComponent In c, ArrowInComponent In c, ArrowCache In Out c)
--       => FixpointCombinator c In Out
-- chaoticInner x = inline chaotic (inline innermost) x
-- {-# SPECIALIZE chaoticInner :: (?cacheWidening :: (W.Widening (Store,Errors), W.Widening (Terminating Val)))
--                      => InterpFix In Out -> InterpFix In Out #-}
