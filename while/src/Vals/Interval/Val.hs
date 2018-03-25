{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Vals.Interval.Val(module Vals.Interval.Val, module Data.Interval) where

import           Data.Interval (Interval)
import qualified Data.Interval as I
import           Data.Order
import           Data.GaloisConnection
import           Data.Powerset
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text (Text)

import qualified Vals.Concrete.Val as Concrete


data Val = BoolVal Bool | NumVal (Interval Double) | Top | Bot
  deriving (Show)

instance PreOrd Val where
  Bot ⊑ _ = True
  _ ⊑ Top = True
  BoolVal b1 ⊑ BoolVal b2 = b1 == b2
  NumVal n1 ⊑ NumVal n2 = n1 ⊑ n2
  _ ⊑ _ = False

instance LowerBounded Val where
  bottom = Bot
instance UpperBounded Val where
  top = Top

instance Complete Val where
  BoolVal b1 ⊔ BoolVal b2 = if b1 == b2 then BoolVal b1 else Top
  NumVal n1 ⊔ NumVal n2 = NumVal $ n1 ⊔ n2
  Bot ⊔ a = a
  a ⊔ Bot = a
  _ ⊔ _ = Top

instance Galois (Pow Concrete.Val) Val where
  alpha = lifted lift
    where lift (Concrete.BoolVal b) = BoolVal b
          lift (Concrete.NumVal n) = NumVal $ I.Interval n n
  gamma Bot = mempty
  gamma (BoolVal b) = return $ Concrete.BoolVal b
  gamma (NumVal (I.Interval m n)) = fromFoldable [Concrete.NumVal x | x <- [m..n]]
  gamma (NumVal I.Bot) = mempty
  gamma Top = gamma (BoolVal True) `union` gamma (BoolVal False) `union` gamma (NumVal top)

