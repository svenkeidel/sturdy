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
  -fsimpl-tick-factor=1000
  -fno-warn-orphans
  -fno-warn-partial-type-signatures
#-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module TypedAnalysis.Chaotic where

import           Prelude hiding (not,Bounded,fail,(.),exp,read,IO)

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Environment as Env
import qualified Control.Arrow.Fix as Fix
import           Control.Arrow.Fix.Chaotic(IterationStrategy,chaotic,innermost',outermost')
import qualified Control.Arrow.Fix.Context as Ctx
import qualified Control.Arrow.Fix.ControlFlow as CFlow
import qualified Control.Arrow.Trans as Trans
import           Control.Arrow.Fix.GarbageCollection as GC
import           Control.Arrow.Transformer.Value
import           Control.Arrow.Transformer.Abstract.LogError
import           Control.Arrow.Transformer.Abstract.Fix
import           Control.Arrow.Transformer.Abstract.Fix.Component as Comp
import           Control.Arrow.Transformer.Abstract.Fix.Context
import           Control.Arrow.Transformer.Abstract.Fix.Stack as Stack
import           Control.Arrow.Transformer.Abstract.Fix.Cache.Immutable as Cache
import           Control.Arrow.Transformer.Abstract.Fix.Metrics as Metric
import           Control.Arrow.Transformer.Abstract.Fix.ControlFlow
import           Control.Arrow.Transformer.Abstract.Environment (EnvT)
import           Control.Arrow.Transformer.Abstract.Store (StoreT)
import           Control.Arrow.Transformer.Abstract.Fix.GarbageCollection


import qualified Data.Abstract.Widening as W
import           Data.Empty
import           Data.Label
import           Data.Text (Text)
import           Data.Abstract.Powerset(Pow) 
import           Data.HashSet (HashSet)
import           Data.HashMap.Lazy (HashMap)
import           Data.Hashed.Lazy (Hashed)


import           TypedAnalysis hiding (Cons)
import           Syntax (LExpr,Expr(App,Cons))
import           GenericInterpreter as Generic

type InterpChaotic x y =
    (ValueT (Pow Val)
      (LogErrorT Text
        (EnvT (Hashed (HashMap Text Addr))
          (StoreT (HashMap Addr (Pow Val))
            (FixT
              (MetricsT Metric.Metrics In
                (ComponentT Comp.Component  In
                  (StackT Stack.Stack In
                    (CacheT Cache.GarbageCollect In Out
                      (ContextT Ctx
                        (GarbageCollectionT Addr 
                          (ControlFlowT Expr
                            (->))))))))))))) x y

evalChaotic :: (?sensitivity :: Int) => IterationStrategy _ In Out -> [(Text,Addr)] -> [LExpr] ->  (CFG Expr, (Metric.Metrics In, Out'))
evalChaotic iterationStrat env0 e =
  let ?cacheWidening = (storeErrWidening_nm, W.finite) in
  let ?fixpointAlgorithm = transform $
        Fix.fixpointAlgorithm $
        -- Fix.trace printIn printOut .        
        Ctx.recordCallsite ?sensitivity (\(_,(_,exprs)) -> case exprs of App _ _ l:_ -> Just l; _ -> Nothing) .
        Fix.recordEvaluated . 
        -- CFlow.recordControlFlowGraph' (\(_,(_,exprs)) -> case exprs of e':_ -> Just e'; _ -> Nothing) .
        -- GC.collectRet (\_ -> True) getAddrIn getAddrOut getReachable removeFromStore .
        -- GC.collectRet gcEntry getAddrIn getAddrOut getReachable removeFromStore .
        -- GC.collectCall gcEntry getAddrIn getAddrOut getReachableIn removeFromStoreIn .
        -- GC.collectCall (\_ -> True) getAddrIn getAddrOut getReachableIn removeFromStoreIn .
        -- GC.trace getAddrIn getAddrOut getReachable printAddr printIn printOut . 
        Fix.filter' isFunctionBody (chaotic iterationStrat)
        in
  second snd $ Trans.run (extend' (Generic.runFixed :: InterpChaotic [Expr] (Pow Val))) (empty,(empty,(env0, e0)))
  where
    e0 = generate (sequence e)
{-# INLINE evalChaotic #-}

evalInner :: Eval_nm
evalInner = evalChaotic innermost'

evalOuter :: Eval_nm
evalOuter = evalChaotic outermost'

evalInner' :: Eval_nm'
evalInner' exprs = let (cfg,(metrics,(_,res))) = evalInner [] exprs in (cfg,(metrics,res))

evalOuter':: Eval_nm'
evalOuter' exprs = let (cfg,(metrics,(_,res))) = evalOuter [] exprs in (cfg,(metrics,res))
