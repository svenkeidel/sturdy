{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Vals.Symbolic.Val where

import WhileLanguage

import           Data.GaloisConnection
import           Data.Powerset

import qualified Vals.Concrete.Val as Concrete

type Val = Expr

instance Galois (Pow Concrete.Val) (Pow Val) where
  alpha = lifted lift
    where lift (Concrete.BoolVal b) = singleton $ BoolLit b @@ 0
          lift (Concrete.NumVal n) = singleton $ NumLit n @@ 0

  gamma = undefined


type Store = ()
initStore :: Store
initStore = ()

