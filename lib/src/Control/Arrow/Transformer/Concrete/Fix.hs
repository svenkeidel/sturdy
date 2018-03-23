{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE CPP #-}
module Control.Arrow.Transformer.Concrete.Fix where

import Prelude hiding ((.))

import Control.Arrow
import Control.Arrow.Fix
import Control.Category

#ifdef TRACE
import           Debug.Trace
import           Text.Printf
#endif

newtype Fix x y = Fix { runFix :: x -> y }
  deriving (Arrow,ArrowChoice,ArrowApply)

liftFix :: (x -> y) -> Fix x y
liftFix = Fix

deriving instance Category Fix

#ifdef TRACE
instance (Show x, Show y) => ArrowFix x y Fix where
  fixA f = Fix (\x -> let y = runFix (f (fixA f)) x in trace (printf "%s <- eval(%s)" (show y) (show x)) y)
#else
instance ArrowFix x y Fix where
  fixA f = Fix (runFix (f (fixA f)))
#endif
