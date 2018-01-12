{-# LANGUAGE DeriveGeneric #-}
module Data.Sign where

import Data.Order
import Data.Hashable
import GHC.Generics

data Sign = Negative | Zero | Positive | Top
  deriving (Show,Eq,Generic)

instance Num Sign where
  Negative + Negative = Negative
  Positive + Positive = Positive
  Zero + x = x
  x + Zero = x
  _ + _ = Top

  Negative * Negative = Positive
  Positive * Negative = Negative
  Negative * Positive = Negative
  Positive * Positive = Positive
  Zero * _ = Zero
  _ * Zero = Zero
  Top * _ = Top
  _ * Top = Top

  negate Zero = Zero
  negate Positive = Negative
  negate Negative = Positive
  negate Top = Top

  abs Zero = Zero
  abs _ = Positive

  signum Negative = Negative
  signum Positive = Positive
  signum Zero = Zero
  signum Top = Top

  fromInteger n
    | n == 0 = Zero
    | n < 0 = Negative
    | otherwise = Positive

instance PreOrd Sign where
  _ ⊑ Top = True
  _ ⊑ _ = False

instance Complete Sign where
  Negative ⊔ Negative = Negative
  Zero ⊔ Zero = Zero
  Positive ⊔ Positive = Positive
  _ ⊔ _ = Top

instance Hashable Sign
