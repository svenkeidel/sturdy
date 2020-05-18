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
import           Control.Arrow.Fix.Chaotic(IterationStrategy,chaotic,innermost',outermost')
import qualified Control.Arrow.Fix.Context as Ctx
import qualified Control.Arrow.Trans as Trans
import           Control.Arrow.Transformer.Value
import           Control.Arrow.Transformer.Abstract.FiniteEnvStore
import           Control.Arrow.Transformer.Abstract.LogError
import           Control.Arrow.Transformer.Abstract.Fix
import           Control.Arrow.Transformer.Abstract.Fix.Component as Comp
import           Control.Arrow.Transformer.Abstract.Fix.Context
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
                   (ContextT Ctx
                     (ControlFlowT Expr
                       c))))))))))) x y

evalChaotic :: (?sensitivity :: Int) => IterationStrategy _ In Out -> [(Text,Addr)] -> [LExpr] -> (CFG Expr, (Metric.Monotone In, Out'))
evalChaotic iterationStrat env0 e =
  let ?cacheWidening = (storeErrWidening, W.finite) in
  let ?fixpointAlgorithm = transform $
        Fix.fixpointAlgorithm $
        -- Fix.trace printIn printOut .
        Ctx.recordCallsite ?sensitivity (\(_,(_,exprs)) -> case exprs of App _ _ l:_ -> Just l; _ -> Nothing) .
        Fix.recordEvaluated .
        -- CFlow.recordControlFlowGraph' (\(_,(_,exprs)) -> case exprs of e':_ -> Just e'; _ -> Nothing) .
        -- Fix.filter' isFunctionBody (Fix.trace printIn printOut . chaotic iterationStrat)
        Fix.filter' isFunctionBody (chaotic iterationStrat) in
  second snd $ Trans.run (extend' (Generic.runFixed :: InterpT (->) [Expr] Val)) (empty,(empty,(env0,e0)))
  where
    e0 = generate (sequence e)
{-# INLINE evalChaotic #-}

evalInner :: Eval
evalInner = evalChaotic innermost'

evalOuter :: Eval
evalOuter = evalChaotic outermost'

evalInner' :: Eval'
evalInner' exprs = let (metrics,(cfg,res)) = evalInner [] exprs in (metrics,(cfg,snd res))

evalOuter':: Eval'
evalOuter' exprs = let (metrics,(cfg,res)) = evalOuter [] exprs in (metrics,(cfg,snd res))

eval' :: (?sensitivity :: Int) => [(Text,Addr)] -> [LExpr] -> (Errors,Terminating Val)
eval' env exprs = snd $ snd $ snd $ evalInner env exprs

data DebugState = DebugState { socket :: Socket, breakpoints :: Breakpoints }

server :: IO ()
server = proc (env,expr) -> do
  socket <- createSocket -< 1234
  loop
  where
    loop = do
      msg <- Client.receive -< ()
      case msg of
        StartProgram file breakpoints -> do
          prog <- loadSchemeFile file
          let ?sensitivity = 0
          let debugState = DebugState socket breakpoints
          evalDebug debugState prog
          loop
        _ ->
          closeSocket socket

evalDebug :: (?sensitivity::Int) => DebugState -> LExpr -> IO ()
evalDebug socket expr =
  let ?cacheWidening = (storeErrWidening, W.finite) in
  let ?fixpointAlgorithm = transform $
       Fix.fixpointAlgorithm $
         debugCombinator socket .
         Ctx.recordCallsite ?sensitivity (\(_,(_,exprs)) -> case exprs of App _ _ l:_ -> Just l; _ -> Nothing) .
         Fix.filter' isFunctionBody (chaotic innermost') in
  _ <- Trans.run (extend' (Generic.runFixed :: InterpChaotic [Expr] Val)) (empty,(empty,([],e0)))
  return ()
  where
    e0 = generate (sequence e)

Client.send :: c Message ()
Client.send = liftIO (\message -> socket.send (encode message))

Socket.send :: JSON -> IO ()


debugCombinator :: ArrowStackElements In c => DebugState -> FixpointCombinator c ((Store,Errors),(Env,[Expr])) ((Store,Errors), Terminating Val)
debugCombinator state f = proc input@((store,errors),(env,expr)) -> do
  if getLabel expr `elem` breakpoints state
    then
      -- We reached a breakpoint
      Client.send -< (reachedBreakpoint (getLabel expr) env)
      loop -< input
      f <- input
    else do
      f -< input
  where
    loop = proc input@((store,errors)(env,expr)) -> do
      msg <- Client.receive -< ()
      case msg of
        ContinueMessage ->
          returnA -< ()
        GetStackMessage -> do
          stack <- Stack.elems -< ()
          Client.send (GetStackResponse (encode stack))
          loop -< input
