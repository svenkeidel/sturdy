{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Arrows #-}
module Control.Arrow.Transformer.Abstract.GreatestFixPoint(GreatestFixPoint(..),runGreatestFixPoint) where

import Control.Arrow
import Control.Arrow.Fix
import Control.Arrow.Reader
import Control.Arrow.Abstract.Join
import Control.Arrow.Transformer.Reader
import Control.Category

import Data.Order

-- | Overapproximates the greatest fixpoint of a computation. The
-- implementation takes an initial amount of 'fuel' and calls the
-- interpreter function recursively until it runs out. In this case
-- 'top' is returned.
newtype GreatestFixPoint a b = GreatestFixPoint (Reader Int (->) a b)

runGreatestFixPoint :: Int -> GreatestFixPoint a b -> a -> b
runGreatestFixPoint k (GreatestFixPoint f) a = runReader f (k,a)

instance UpperBounded b => ArrowFix a b GreatestFixPoint where
  fix f = proc x -> do
    i <- askFuel -< ()
    if i <= 0
      then top -< ()
      else localFuel (f (fix f)) -< (i-1,x)

askFuel :: GreatestFixPoint () Int
askFuel = GreatestFixPoint ask

localFuel :: GreatestFixPoint a b -> GreatestFixPoint (Int, a) b
localFuel (GreatestFixPoint f) = GreatestFixPoint $ proc (i,a) -> local f -< (i,a)

deriving instance Category GreatestFixPoint
deriving instance Arrow GreatestFixPoint
deriving instance ArrowChoice GreatestFixPoint
deriving instance ArrowApply GreatestFixPoint
deriving instance ArrowJoin GreatestFixPoint

deriving instance PreOrd b => PreOrd (GreatestFixPoint a b)
deriving instance Complete b => Complete (GreatestFixPoint a b)
deriving instance CoComplete b => CoComplete (GreatestFixPoint a b)
deriving instance UpperBounded b => UpperBounded (GreatestFixPoint a b)
deriving instance LowerBounded b => LowerBounded (GreatestFixPoint a b)
