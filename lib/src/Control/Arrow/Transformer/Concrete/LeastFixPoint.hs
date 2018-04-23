{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Transformer.Concrete.LeastFixPoint(Fix,runLeastFixPoint) where

import Prelude hiding ((.))

import Control.Arrow.Fix

type instance Fix a b (->) = (->)

runLeastFixPoint :: Fix a b (->) x y -> x -> y
runLeastFixPoint f = f
