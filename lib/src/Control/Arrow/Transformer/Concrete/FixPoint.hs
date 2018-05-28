{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Transformer.Concrete.FixPoint(Fix,runFixPoint) where

import Prelude hiding ((.))

import Control.Arrow.Fix

-- | Arrow transformer that computes the fixpoint in the concrete interpreter.
type instance Fix a b (->) = (->)

-- | Excecutes a concrete fixpoint computation.
runFixPoint :: Fix a b (->) x y -> x -> y
runFixPoint f = f
