{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Arrows #-}
module Control.Arrow.Transformer.GreatestFixPoint(GreatestFixPoint(..)) where

import Control.Arrow
import Control.Arrow.Fix
import Control.Arrow.Reader
import Control.Arrow.Transformer.Reader
import Control.Category

import Data.Order

newtype GreatestFixPoint a b = GreatestFixPoint (Reader Int (->) a b)

deriving instance Arrow GreatestFixPoint
deriving instance ArrowChoice GreatestFixPoint
deriving instance ArrowReader Int GreatestFixPoint
deriving instance Category GreatestFixPoint

deriving instance PreOrd b => PreOrd (GreatestFixPoint a b)
deriving instance Complete b => Complete (GreatestFixPoint a b)
deriving instance CoComplete b => CoComplete (GreatestFixPoint a b)
deriving instance UpperBounded b => UpperBounded (GreatestFixPoint a b)
deriving instance LowerBounded b => LowerBounded (GreatestFixPoint a b)

instance UpperBounded b => ArrowFix a b GreatestFixPoint where
  fixA f = proc x -> do
    i <- askA -< ()
    if i <= 0
      then top -< ()
      else localFuel (f (fixA f)) -< (i-1,x)

localFuel :: GreatestFixPoint a b -> GreatestFixPoint (Int, a) b
localFuel (GreatestFixPoint f) = GreatestFixPoint $ proc (i,a) -> localA f -< (i,a)
