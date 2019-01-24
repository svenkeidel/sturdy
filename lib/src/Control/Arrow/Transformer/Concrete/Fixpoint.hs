{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Arrow.Transformer.Concrete.Fixpoint(FixT,runFixT) where

import Prelude hiding ((.))

import Control.Arrow.Fix

import Control.Category
import Control.Arrow

-- | Arrow transformer that computes the fixpoint in the concrete interpreter.
newtype FixT a b c x y = FixT {runFixT :: c x y} deriving (Category,Arrow,ArrowChoice)

type instance Fix x y (FixT () () c) = FixT x y c
instance Arrow c => ArrowFix x y (FixT x y c) where
  fix f = FixT $ runFixT (f (fix f))

