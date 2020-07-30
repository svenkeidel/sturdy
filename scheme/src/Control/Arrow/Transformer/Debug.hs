{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}


module Control.Arrow.Transformer.Debug where

import           Control.Category
import           Control.Arrow hiding (loop)
import           Control.Arrow.Primitive
import           Control.Arrow.Strict
import           Control.Arrow.Fix
import           Control.Arrow.Fix.Cache
import           Control.Arrow.Fix.Chaotic
import           Control.Arrow.Fix.ControlFlow
import           Control.Arrow.Fix.Context
import           Control.Arrow.Fix.Metrics
import           Control.Arrow.Fix.Stack as Stack
import           Control.Arrow.Order(ArrowComplete(..),ArrowJoin(..))
import           Control.Arrow.Trans
import           Control.Arrow.IO

import           Data.Profunctor
import           Data.Profunctor.Unsafe((.#))
import           Data.Coerce
import           Data.Order hiding (lub)


import           Syntax (LExpr,Expr(App))



import qualified Network.HTTP.Types             as Http
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets             as WS
import qualified Safe

import qualified Control.Concurrent             as Concurrent

import          Control.Arrow.State as State

import          Control.Arrow.Transformer.State
import           Prelude hiding (lookup,read,fail,Bounded(..))




import           Control.Arrow.Fix.Parallel (parallel,adi)
import           Control.Arrow.Transformer.Abstract.Terminating
import           Control.Arrow.Transformer.Abstract.Fix
import           Control.Arrow.Transformer.Abstract.Fix.Metrics
import           Control.Arrow.Transformer.Abstract.Fix.Component
import           Control.Arrow.Transformer.Abstract.Fix.Cache.Immutable hiding (Widening)
import           Control.Arrow.Transformer.Abstract.Fix.Stack (Stack,StackT)

import qualified Data.Text                      as Text
import           Data.Identifiable

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M

import Data.Monoidal
import           Data.Abstract.MonotoneStore(Store)

import Data.Graph.Inductive.Graph(mkGraph, LNode, LEdge, labNodes, labEdges, Graph)


type ClientId = Int
type Client   = (ClientId, WS.Connection)
type State    = [Client]

type Socket = () -- Placeholder
type Breakpoints = [Expr] -- Placeholder
--type StackElems = [In] -- Placeholder

data DebugState = DebugState {
  breakpoints :: Breakpoints,  
  conn :: WS.Connection,
  clientId :: ClientId,
  stateRef :: Concurrent.MVar State,
  expressionList :: [LExpr],
  debugPhase :: Int
}


newtype DebugT c x y = DebugT (StateT DebugState c x y)
  deriving (Profunctor,Category,Arrow,ArrowChoice,
            ArrowContext ctx, ArrowJoinContext a, ArrowControlFlow a,
            ArrowCache a b, ArrowParallelCache a b, ArrowIterateCache a b, ArrowGetCache cache,
            ArrowStack a,ArrowStackElements a,ArrowStackDepth,
            ArrowComponent a, ArrowInComponent a,
            ArrowMetrics a, ArrowStrict, ArrowPrimitive, ArrowCFG a)



class ArrowDebug c where
  addBreakpoints :: c Breakpoints ()
  isBreakpoint :: c Expr Bool
  sendMessage :: c Text.Text ()
  receiveMessage :: c () Text.Text
  sendStack :: c Text.Text ()




instance (Profunctor c, Arrow c, ArrowRun c) => ArrowRun (DebugT c) where
  type Run (DebugT c) x y = Run c (DebugState,x) (DebugState,y)
  run (DebugT (StateT f)) = run f





deriving instance ArrowDebug c => ArrowDebug (FixT c)
instance (Arrow c, Profunctor c, ArrowIO c) => ArrowDebug (DebugT c) where
  addBreakpoints = DebugT $ proc bps -> do
    state <- State.get -< ()
    State.put -< state { breakpoints = bps ++ breakpoints state }
    returnA -< ()
  isBreakpoint = DebugT $ proc expr -> do
    state <- State.get -< ()
    returnA -< expr `Prelude.elem` (breakpoints state)
  sendMessage = DebugT $ proc message -> do
    state <- State.get -< ()
    liftIO sendResponse -< (state,message)
    returnA -< ()
  receiveMessage = DebugT $ proc message -> do
    state <- State.get -< ()
    msg <- liftIO WS.receiveData -< (conn state)
    returnA -< msg
  sendStack = DebugT $ proc message-> do
    state <- State.get -< ()
    --stack <- Stack.elems -< ()
    returnA-< ()


  {-# INLINE addBreakpoints #-}
  {-# INLINE isBreakpoint #-}
  {-# INLINE sendMessage #-}
  {-# INLINE receiveMessage #-}
  {-# INLINE sendStack #-}

sendResponse :: (DebugState,Text.Text) -> IO ()
sendResponse (debugState,msg)= do
  WS.sendTextData (conn debugState) msg

