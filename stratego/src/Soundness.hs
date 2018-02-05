{-# LANGUAGE FlexibleContexts #-}
module Soundness where

import           ConcreteSemantics
import           Syntax

import           Data.Powerset (Pow)
import           Data.GaloisConnection
import           Data.Hashable

import           Test.QuickCheck

class Soundness c' where
  sound :: (Eq x, Eq x', Eq y, Eq y', Show x, Show y, Show x', Show y',
            Hashable x, Hashable x', Hashable y, Hashable y',
            Galois (Pow x) x', Galois (Pow y) y')
        => StratEnv -> Pow (x,TermEnv) -> Interp x y -> c' x' y' -> Property



