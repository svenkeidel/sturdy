{-# LANGUAGE DeriveGeneric #-}
module Data.Sign where

import Data.Order
import Data.Hashable
import Data.Widening

import GHC.Generics

data Sign = Bot | Negative | Zero | Positive | Top
  deriving (Show,Eq,Generic)

instance Num Sign where
  Bot + _ = Bot
  _ + Bot = Bot
  Negative + Negative = Negative
  Positive + Positive = Positive
  Zero + x = x
  x + Zero = x
  _ + _ = Top

  Bot * _ = Bot
  _ * Bot = Bot
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
  negate Bot = Bot

  abs Zero = Zero
  abs _ = Positive

  signum Negative = Negative
  signum Positive = Positive
  signum Zero = Zero
  signum Top = Top
  signum Bot = Bot

  fromInteger n
    | n == 0 = Zero
    | n < 0 = Negative
    | otherwise = Positive

instance PreOrd Sign where
  Bot ⊑ _ = True
  _ ⊑ Top = True
  Negative ⊑ Negative = True
  Zero ⊑ Zero = True
  Positive ⊑ Positive = True
  _ ⊑ _ = False

instance Complete Sign where
  Bot ⊔ y = y
  x ⊔ Bot = x
  Negative ⊔ Negative = Negative
  Zero ⊔ Zero = Zero
  Positive ⊔ Positive = Positive
  _ ⊔ _ = Top

instance Widening Sign

instance LowerBounded Sign where
  bottom = Bot

instance UpperBounded Sign where
  top = Top

instance Hashable Sign
