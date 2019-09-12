{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Abstract.Sign where

import           Prelude hiding ((==),(/),Bool(..))
import qualified Prelude as P

import           Data.Order
import           Data.Hashable
import           Data.Numeric

import           Data.Abstract.Equality
import           Data.Abstract.Widening
import           Data.Abstract.Stable
import           Data.Abstract.Failure
import qualified Data.Abstract.Boolean as B

import           GHC.Generics

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
  abs Positive = Positive
  abs Negative = Positive
  abs Top = Top

  signum Negative = Negative
  signum Positive = Positive
  signum Zero = Zero
  signum Top = Top

  fromInteger n
    | n P.== 0 = Zero
    | n < 0 = Negative
    | otherwise = Positive

instance Numeric Sign (Failure String) where
  Negative / Negative = Success Positive
  Positive / Negative = Success Negative
  Negative / Positive = Success Negative
  Positive / Positive = Success Positive
  _ / Top  = Fail "divided by 0" ⊔ Success Top
  Top / _  = Success Top
  _ / Zero = Fail "divided by 0"
  Zero / _ = Success Zero

instance Equality Sign where
  Zero == Zero = B.True
  Negative == Negative = B.Top
  Positive == Positive = B.Top
  Top == _ = B.Top
  _ == Top = B.Top
  _ == _ = B.False

instance PreOrd Sign where
  _ ⊑ Top = P.True
  Negative ⊑ Negative = P.True
  Zero ⊑ Zero = P.True
  Positive ⊑ Positive = P.True
  _ ⊑ _ = P.False

instance Complete Sign where
  Negative ⊔ Negative = Negative
  Zero ⊔ Zero = Zero
  Positive ⊔ Positive = Positive
  _ ⊔ _ = Top

instance UpperBounded Sign where
  top = Top

instance Hashable Sign

widening :: Widening Sign
widening Negative Negative = (Stable,Negative)
widening Zero Zero = (Stable,Zero)
widening Positive Positive = (Stable,Positive)
widening Top Top = (Stable,Positive)
widening _ _ = (Unstable,Top)
