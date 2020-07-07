{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Transformer.Abstract.Fix.ControlFlow where

import           Prelude hiding(pred,(.))

import           Control.Arrow
import           Control.Arrow.Trans
import           Control.Arrow.Transformer.State
import           Control.Arrow.Transformer.Reader
import           Control.Arrow.Fix.Cache as Cache
import           Control.Arrow.Fix.Context (ArrowContext)
import           Control.Arrow.Fix.ControlFlow
import           Control.Category
import           Control.Arrow.Fix.GarbageCollection (ArrowGarbageCollection)

import           Data.Label
import           Data.Coerce
import           Data.Empty
import           Data.Profunctor.Unsafe
import           Data.Graph.Inductive (Gr)
import qualified Data.Graph.Inductive as G

newtype CFG stmt = CFG (Gr stmt ())

instance IsEmpty (CFG stmt) where
  empty = CFG G.empty

newtype ControlFlowT stmt c x y = ControlFlowT (StateT (CFG stmt) (ReaderT (Maybe stmt) c) x y)
  deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowContext ctx,
            ArrowGarbageCollection addr, ArrowCache a b, ArrowParallelCache a b, 
            ArrowIterateCache a b)

instance (HasLabel stmt, Arrow c, Profunctor c) => ArrowControlFlow stmt (ControlFlowT stmt c) where
  nextStatement f = lift $ proc (predecessor, (cfg, (nextStmt, x))) ->
    unlift f -< (nextStmt, (addEdge predecessor nextStmt cfg, x))
    where
      addEdge :: HasLabel stmt => Maybe stmt -> Maybe stmt -> CFG stmt -> CFG stmt
      addEdge pred next (CFG graph) = CFG $ case (pred,next) of
        (Just pred', Just next') -> insEdge pred' next' graph
        _ -> graph

      insEdge :: HasLabel stmt => stmt -> stmt -> Gr stmt () -> Gr stmt ()
      insEdge pred next gr
        | G.hasEdge gr (lp,ln) = gr
        | otherwise            = G.insEdge (lp, ln, ()) $ insNode pred $ insNode next gr
        where
          lp = labelVal $ label pred
          ln = labelVal $ label next

      insNode :: HasLabel stmt => stmt -> Gr stmt () -> Gr stmt ()
      insNode stmt gr = case G.lab gr (labelVal (label stmt)) of
        Just _ -> gr
        Nothing -> G.insNode (labelVal $ label stmt, stmt) gr
  {-# INLINE nextStatement #-}

instance (ArrowRun c) => ArrowRun (ControlFlowT stmt c) where
  type Run (ControlFlowT stmt c) x y = Run c x (CFG stmt,y)
  run f = run (lmap (\x ->(Nothing,(empty,x))) (unlift f))
  {-# INLINE run #-}

instance ArrowTrans (ControlFlowT stmt) where
  lift' = ControlFlowT . lift' . lift'
  {-# INLINE lift' #-}

instance ArrowLift (ControlFlowT stmt c) where
  type Underlying (ControlFlowT stmt c) x y = c (Maybe stmt, (CFG stmt,x)) (CFG stmt,y)

instance (Profunctor c,ArrowApply c) => ArrowApply (ControlFlowT stmt c) where
  app = ControlFlowT (app .# first coerce)
  {-# INLINE app #-}