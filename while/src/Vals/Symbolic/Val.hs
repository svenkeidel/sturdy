{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Vals.Symbolic.Val where

import WhileLanguage

import           Data.Interval (Interval)
import qualified Data.Interval as I
import           Data.Order
import           Data.GaloisConnection
import           Data.Powerset
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text (Text)

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

