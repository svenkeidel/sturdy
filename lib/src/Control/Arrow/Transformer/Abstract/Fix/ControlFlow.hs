{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Control.Arrow.Transformer.Abstract.Fix.ControlFlow where

import           Prelude hiding(pred)

import           Control.Arrow
import           Control.Arrow.Order
import           Control.Arrow.Trans
import           Control.Arrow.Transformer.State
import           Control.Arrow.Fix.ControlFlow
import           Control.Category


import           Data.Label
import           Data.Coerce
import           Data.Empty
import           Data.Order
import           Data.Profunctor.Unsafe
import           Data.Graph.Inductive (Gr)
import qualified Data.Graph.Inductive as G

data CFG stmt = CFG
  { graph :: Gr stmt ()
  , predecessor :: Maybe stmt
  }

instance IsEmpty (CFG stmt) where
  empty = CFG { graph = G.empty
              , predecessor = Nothing
              }

newtype ControlFlowT stmt c x y = ControlFlowT (StateT (CFG stmt) c x y)
  deriving (Profunctor,Category,Arrow,ArrowChoice)

instance (HasLabel stmt, Arrow c, Profunctor c) => ArrowControlFlow stmt (ControlFlowT stmt c) where
  nextStatement = lift $ proc (cfg,nextStmt) -> do
    let cfg' = cfg { graph = addEdge (predecessor cfg) nextStmt (graph cfg)
                   , predecessor = Just nextStmt
                   }
    returnA -< (cfg', ())
    where
      addEdge :: HasLabel stmt => Maybe stmt -> stmt -> Gr stmt () -> Gr stmt ()
      addEdge pred next cfg = case pred of  
        Just pred' -> do 
          let cfg' = G.insEdge (labelVal $ label pred', labelVal $ label next, ()) cfg
          G.insNode (labelVal $ label next, next) cfg'
        Nothing -> G.insNode (labelVal $ label next, next) cfg

instance (ArrowRun c) => ArrowRun (ControlFlowT stmt c) where
  type Run (ControlFlowT stmt c) x y = Run c x (Gr stmt (),y)
  run f = run (dimap (empty,) (first graph) (unlift f))
  {-# INLINE run #-}

instance ArrowTrans (ControlFlowT stmt c) where
  type Underlying (ControlFlowT stmt c) x y = c (CFG stmt,x) (CFG stmt,y)

instance ArrowEffectCommutative c => ArrowEffectCommutative (ControlFlowT stmt c)

instance (Complete y, ArrowEffectCommutative c) => ArrowComplete y (ControlFlowT stmt c) where
  ControlFlowT f <⊔> ControlFlowT g = ControlFlowT $ rmap (uncurry (⊔)) (f &&& g)
  {-# INLINE (<⊔>) #-}

instance (Profunctor c,ArrowApply c) => ArrowApply (ControlFlowT stmt c) where
  app = ControlFlowT (app .# first coerce)
  {-# INLINE app #-}
