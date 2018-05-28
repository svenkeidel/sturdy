{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Transformer.Const where

import Prelude hiding (id,(.),lookup,read)

import Control.Category

import Control.Arrow
import Control.Arrow.Environment
import Control.Arrow.Fail
import Control.Arrow.Except
import Control.Arrow.Fix
import Control.Arrow.Lift
import Control.Arrow.Reader
import Control.Arrow.State
import Control.Arrow.Store
import Control.Arrow.Const
import Control.Arrow.Writer
import Control.Arrow.Transformer.Static
    
import Data.Order

-- | Passes along constant data.
newtype Const r c x y = Const (Static ((->) r) c x y)

runConst :: r -> Const r c x y -> c x y
runConst r (Const (Static f)) = f r

type instance Fix x y (Const r c) = Const r (Fix x y c)
instance ArrowFix x y c => ArrowFix x y (Const r c) where
  fixA f = Const $ Static $ \r -> fixA (runConst r . f . lift)

instance Arrow c => ArrowConst r (Const r c) where
  askConst = Const $ Static $ \r -> arr (const r)

instance ArrowApply c => ArrowApply (Const r c) where
  app = Const $ Static $ \r -> (\(Const (Static f),x) -> (f r,x)) ^>> app

deriving instance ArrowLift (Const r)
deriving instance Arrow c => Category (Const r c)
deriving instance Arrow c => Arrow (Const r c)
deriving instance ArrowChoice c => ArrowChoice (Const r c)
deriving instance ArrowLoop c => ArrowLoop (Const r c)
deriving instance ArrowState s c => ArrowState s (Const r c)
deriving instance ArrowReader r c => ArrowReader r (Const r' c)
deriving instance ArrowWriter w c => ArrowWriter w (Const r c)
deriving instance ArrowEnv x y env c => ArrowEnv x y env (Const r c)
deriving instance ArrowStore var val lab c => ArrowStore var val lab (Const r c)
deriving instance ArrowFail e c => ArrowFail e (Const r c)
deriving instance ArrowExcept x y e c => ArrowExcept x y e (Const r c)

deriving instance PreOrd (c x y) => PreOrd (Const r c x y)
deriving instance Complete (c x y) => Complete (Const r c x y)
deriving instance CoComplete (c x y) => CoComplete (Const r c x y)
deriving instance UpperBounded (c x y) => UpperBounded (Const r c x y)
deriving instance LowerBounded (c x y) => LowerBounded (Const r c x y)
