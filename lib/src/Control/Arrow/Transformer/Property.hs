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

newtype PropertyArrow p c x y = PropertyArrow (State p c x y) 

instance ArrowLift (PropertyArrow r) where
  lift f = PropertyArrow (lift f)

runProperty :: PropertyArrow p c x y -> c (p,x) (p,y)
runProperty (PropertyArrow (State f)) = f

instance Arrow c => HasProp p (PropertyArrow p c) where
  modifyProp (PropertyArrow (State f)) =
    PropertyArrow (State ((\(p,x) -> (p,(x,p))) ^>> f >>^ (\(_,p) -> (p,()))))

deriving instance Category c => Category (PropertyArrow p c) 
deriving instance Arrow c => Arrow (PropertyArrow p c) 
deriving instance ArrowChoice c => ArrowChoice (PropertyArrow p c) 
deriving instance ArrowReader r c => ArrowReader r (PropertyArrow p c)
deriving instance ArrowFail e c => ArrowFail e (PropertyArrow p c)

instance ArrowApply c => ArrowApply (PropertyArrow p c) where
  app = PropertyArrow (State (arr (\(p,(PropertyArrow (State f),b)) -> (f,(p,b))) >>> app))

instance ArrowState s c => ArrowState s (PropertyArrow p c) where
  getA = lift getA
  putA = lift putA

instance ArrowFix (p,x) (p,y) c => ArrowFix x y (PropertyArrow p c) where
  fixA f = PropertyArrow (State (fixA (runProperty . f . PropertyArrow . State)))

