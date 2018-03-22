{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE CPP #-}
module Control.Arrow.Transformer.Fix where

import Prelude hiding ((.))

import Control.Arrow
import Control.Arrow.Class.Fix
import Control.Category

import Data.Order

#ifdef TRACE
import           Debug.Trace
import           Text.Printf
#endif

newtype Fix a b x y = Fix { runFix :: x -> y }
  deriving (Arrow,ArrowChoice,ArrowApply)

liftFix :: (x -> y) -> Fix a b x y
liftFix = Fix

deriving instance Category (Fix a b)

#ifdef TRACE
instance (Show x, Show y) => ArrowFix x y (Fix x y) where
  fixA f = Fix (\x -> let y = runFix (f (fixA f)) x in trace (printf "%s <- eval(%s)" (show y) (show x)) y)
#else
instance ArrowFix x y (Fix x y) where
  fixA f = Fix (runFix (f (fixA f)))
#endif

deriving instance PreOrd y => PreOrd (Fix a b x y)
deriving instance LowerBounded y => LowerBounded (Fix a b x y)
deriving instance Complete y => Complete (Fix a b x y)

--deriving instance CoComplete y => CoComplete (Fix a b x y)
instance CoComplete y => CoComplete (Fix a b x y) where
  Fix f ⊓ Fix g = Fix (\x -> f x ⊓ g x)

--deriving instance UpperBounded y => UpperBounded (Fix a b x y)
instance UpperBounded y => UpperBounded (Fix a b x y) where
  top = Fix $ const top
