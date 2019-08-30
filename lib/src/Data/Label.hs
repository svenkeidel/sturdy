{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Label where

import Data.Hashable
import Data.Order
import Data.Abstract.FreeCompletion
import Control.Monad.State

-- Retrieves label from expression.
class HasLabel x where
  label :: x -> Label

newtype Label = Label { labelVal :: Int }
  deriving (Ord,Eq,Hashable,Num)

instance Show Label where
  show (Label l) = show l

instance PreOrd Label where
  (⊑) = (==)

instance Complete (FreeCompletion Label) where
  Lower l1 ⊔ Lower l2 | l1 == l2 = Lower l1
  _ ⊔ _ = Top

instance UpperBounded (FreeCompletion Label) where
  top = Top

fresh :: State Label Label
fresh = state (\l -> (l,l+1))

generate :: State Label x -> x
generate m = evalState m 0
