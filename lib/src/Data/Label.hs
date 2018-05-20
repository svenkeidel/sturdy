{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Label where

import Data.Hashable
import Data.Order
import Control.Monad.State

-- Retrieves label from expression.
class HasLabel x where
  label :: x -> Label

newtype Label = Label { labelVal :: Int }
  deriving (Ord,Eq,Hashable,Num)

instance Show Label where
  show (Label l) = show l

fresh :: State Label Label
fresh = state (\l -> (l,l+1))

generate :: State Label x -> x
generate m = evalState m 0

instance PreOrd Label where
  (⊑) = (<=)
  (≈) = (==)
