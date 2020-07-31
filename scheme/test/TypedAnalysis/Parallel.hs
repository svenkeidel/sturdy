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
import           Control.Arrow 
import           Control.Arrow.Environment as Env
import           Control.Arrow.Fix(FixpointAlgorithm,FixpointCombinator)
import qualified Control.Arrow.Fix as Fix
import           Control.Arrow.Fix.Cache(ArrowCache,ArrowParallelCache,Widening)
import           Control.Arrow.Fix.Parallel as Par
import qualified Control.Arrow.Fix.Context as Ctx
import qualified Control.Arrow.Trans as Trans
import           Control.Arrow.Transformer.Value
import           Control.Arrow.Transformer.Abstract.LogError
import           Control.Arrow.Transformer.Abstract.Fix
import           Control.Arrow.Transformer.Abstract.Fix.Context
import           Control.Arrow.Transformer.Abstract.Fix.Cache.Immutable as Cache
import           Control.Arrow.Transformer.Abstract.Fix.Metrics as Metric
import           Control.Arrow.Transformer.Abstract.Fix.ControlFlow
import qualified Control.Arrow.Fix.ControlFlow as CFlow
import           Control.Arrow.Transformer.Abstract.Environment
import           Control.Arrow.Transformer.Abstract.Store
import           Control.Arrow.Transformer.Reader (ReaderT)

import           Control.Monad.State hiding (lift,fail)

import           Data.Empty
import           Data.Label
import           Data.Text (Text)
import           Data.Abstract.Powerset(Pow)
import           Data.HashSet(HashSet)
import           Data.HashMap.Lazy (HashMap)
import Data.Hashed.Lazy (Hashed)

-- import           Data.Text.Prettyprint.Doc

import qualified Data.Abstract.Widening as W


import           Syntax (Expr(App))
import qualified GenericInterpreter as Generic
import           TypedAnalysis

type Interp x y =
  (ValueT (Pow Val)
    (LogErrorT Text
      (EnvT (Hashed (HashMap Text Addr ))
        (StoreT (HashMap Addr (Pow Val))
          (FixT
            (MetricsT Metric.Metrics In
              (CacheT (Parallel Cache.GarbageCollect) In Out
                (ContextT Ctx
                  (-- GarbageCollectionT Addr
                    (ControlFlowT Expr
                      (ReaderT (HashSet Addr)
                        (->)))))))))))) x y  

eval :: (?sensitivity :: Int)
     => (forall c. (?cacheWidening :: Widening c, ArrowChoice c, ArrowCache In Out c, ArrowParallelCache In Out c) =>
                   (FixpointCombinator c In Out -> FixpointCombinator c In Out) -> FixpointAlgorithm (c In Out))
     -> [(Text,Addr)] -> [State Label Expr] -> (CFG Expr, (Metric.Metrics In, Out'))
eval algo env0 e =
  let ?cacheWidening = (storeErrWidening_nm, W.finite) in
  let ?fixpointAlgorithm = transform $ algo $ \update_ ->
        -- Fix.trace printIn printOut .
        -- GC.trace getAddrIn getAddrOut getReachable
        --          printAddr printIn printOut . 
        Ctx.recordCallsite ?sensitivity (\(_,(_,exprs)) -> case exprs of App _ _ l:_ -> Just l; _ -> Nothing) .
        Fix.recordEvaluated .
        CFlow.recordControlFlowGraph' (\(_,(_,exprs)) -> case exprs of e':_ -> Just e'; _ -> Nothing) .
        Fix.filter' isFunctionBody update_ in
  second snd $ Trans.run (extend' (Generic.runFixed :: Interp [Expr] (Pow Val))) (empty,(empty,(empty,(env0,e0))))
  where
    e0 = generate (sequence e)
{-# INLINE eval #-}

evalParallel :: Eval
evalParallel = eval Par.parallel

evalADI :: Eval
evalADI = eval Par.adi

evalParallel':: Eval'
evalParallel' exprs = let (cfg,(metrics,(_,res))) = evalParallel [] exprs in (cfg,(metrics,res))

evalADI':: Eval'
evalADI' exprs = let (cfg,(metrics,(_,res))) = evalADI [] exprs in (cfg,(metrics,res))
