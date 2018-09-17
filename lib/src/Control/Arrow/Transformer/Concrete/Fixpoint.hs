{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Transformer.Concrete.Fixpoint(Fix,runFix) where

import Prelude hiding ((.))

import Control.Arrow.Fix

-- | Arrow transformer that computes the fixpoint in the concrete interpreter.
type instance Fix a b (->) = (->)

-- | Excecutes a concrete fixpoint computation.
runFix :: Fix a b (->) x y -> x -> y
runFix f = f
