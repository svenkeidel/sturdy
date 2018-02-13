{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
module Control.Arrow.Transformer.Fix where

import Control.Arrow
import Control.Arrow.Class.Fix
import Control.Category

import           Data.Order

newtype Fix a b x y = Fix { runFix :: x -> y }
  deriving (Arrow,ArrowChoice,ArrowApply)

deriving instance Category (Fix a b)

instance ArrowFix x y (Fix x y) where
  fixA f = Fix (runFix (f (fixA f)))

deriving instance PreOrd y => PreOrd (Fix a b x y)
deriving instance LowerBounded y => LowerBounded (Fix a b x y)
deriving instance Complete y => Complete (Fix a b x y)

--deriving instance CoComplete y => CoComplete (Fix a b x y)
instance CoComplete y => CoComplete (Fix a b x y) where
  Fix f ⊓ Fix g = Fix (\x -> f x ⊓ g x)

--deriving instance UpperBounded y => UpperBounded (Fix a b x y)
instance UpperBounded y => UpperBounded (Fix a b x y) where
  top = Fix $ const top
