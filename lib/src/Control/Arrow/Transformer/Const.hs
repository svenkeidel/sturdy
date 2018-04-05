{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Transformer.Const where

import Prelude hiding (id,(.),lookup,read)

import Control.Category

import Control.Arrow
import Control.Arrow.Environment
import Control.Arrow.Fail
import Control.Arrow.Fix
import Control.Arrow.Lift
import Control.Arrow.Reader
import Control.Arrow.State
import Control.Arrow.Store
import Control.Arrow.Const
import Control.Arrow.Transformer.Static
    
import Data.Order

-- Passes along _static_ data.
newtype Const r c x y = Const (Static ((->) r) c x y)

runConst :: r -> Const r c x y -> c x y
runConst r (Const (Static f)) = f r

instance ArrowFix x y c => ArrowFix x y (Const r c) where
  fixA f = Const $ Static $ \r -> fixA (runConst r . f . lift)

instance Arrow c => ArrowConst r (Const r c) where
  askConst = Const $ Static $ \r -> arr (const r)

deriving instance ArrowLift (Const r)
deriving instance Arrow c => Category (Const r c)
deriving instance Arrow c => Arrow (Const r c)
deriving instance ArrowChoice c => ArrowChoice (Const r c)
deriving instance ArrowState s c => ArrowState s (Const r c)
deriving instance ArrowReader r c => ArrowReader r (Const r' c)
deriving instance ArrowFail e c => ArrowFail e (Const r c)
deriving instance ArrowEnv x y env c => ArrowEnv x y env (Const r c)
deriving instance ArrowStore var val c => ArrowStore var val (Const r c)
deriving instance PreOrd (c x y) => PreOrd (Const r c x y)
deriving instance Complete (c x y) => Complete (Const r c x y)
deriving instance CoComplete (c x y) => CoComplete (Const r c x y)
deriving instance UpperBounded (c x y) => UpperBounded (Const r c x y)
deriving instance LowerBounded (c x y) => LowerBounded (Const r c x y)
