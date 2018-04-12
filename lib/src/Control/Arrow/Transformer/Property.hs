{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Transformer.Property where

import Prelude hiding ((.))

import Control.Arrow
import Control.Arrow.Fail
import Control.Arrow.Fix
import Control.Arrow.Lift
import Control.Arrow.Property
import Control.Arrow.Reader
import Control.Arrow.State
import Control.Arrow.Transformer.State

import Control.Category

newtype Property p c x y = Property (State p c x y) 

property :: c (p,x) (p,y) -> Property p c x y
property = Property . State

instance ArrowLift (Property r) where
  lift f = Property (lift f)

runProperty :: Property p c x y -> c (p,x) (p,y)
runProperty (Property (State f)) = f

instance Arrow c => HasProp p (Property p c) where
  modifyProp (Property (State f)) =
    Property (State ((\(p,x) -> (p,(x,p))) ^>> f >>^ (\(_,p) -> (p,()))))

deriving instance Category c => Category (Property p c) 
deriving instance Arrow c => Arrow (Property p c) 
deriving instance ArrowChoice c => ArrowChoice (Property p c) 
deriving instance ArrowReader r c => ArrowReader r (Property p c)
deriving instance ArrowFail e c => ArrowFail e (Property p c)

instance ArrowApply c => ArrowApply (Property p c) where
  app = Property (State (arr (\(p,(Property (State f),b)) -> (f,(p,b))) >>> app))

instance ArrowState s c => ArrowState s (Property p c) where
  getA = lift getA
  putA = lift putA

instance ArrowFix (p,x) (p,y) c => ArrowFix x y (Property p c) where
  fixA f = Property (State (fixA (runProperty . f . Property . State)))

