{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Transformer.Concrete.Fix(Fix,runFix) where

import Prelude hiding ((.))

import Control.Arrow.Fix

type instance Fix a b (->) = (->)

runFix :: Fix a b (->) x y -> x -> y
runFix f = f
