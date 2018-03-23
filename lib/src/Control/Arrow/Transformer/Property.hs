{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Transformer.Property where

import Prelude hiding ((.))

import Control.Arrow
import Control.Arrow.Transformer.State
import Control.Arrow.Class.State
import Control.Arrow.Class.Reader
import Control.Arrow.Class.Fail
import Control.Arrow.Class.Fix
import Control.Arrow.Class.Property

import Control.Category

newtype PropertyArrow p c x y = PropertyArrow (StateArrow p c x y) 

liftProperty :: Arrow c => c x y -> PropertyArrow p c x y
liftProperty f = PropertyArrow (liftState f)

runProperty :: PropertyArrow p c x y -> c (p,x) (p,y)
runProperty (PropertyArrow (StateArrow f)) = f

instance Arrow c => HasProp p (PropertyArrow p c) where
  modifyProp (PropertyArrow (StateArrow f)) =
    PropertyArrow (StateArrow ((\(p,x) -> (p,(x,p))) ^>> f >>^ (\(_,p) -> (p,()))))

deriving instance Category c => Category (PropertyArrow p c) 
deriving instance Arrow c => Arrow (PropertyArrow p c) 
deriving instance ArrowChoice c => ArrowChoice (PropertyArrow p c) 
deriving instance ArrowReader r c => ArrowReader r (PropertyArrow p c)
deriving instance ArrowFail e c => ArrowFail e (PropertyArrow p c)

instance ArrowApply c => ArrowApply (PropertyArrow p c) where
  app = PropertyArrow (StateArrow (arr (\(p,(PropertyArrow (StateArrow f),b)) -> (f,(p,b))) >>> app))

instance ArrowState s c => ArrowState s (PropertyArrow p c) where
  getA = liftProperty getA
  putA = liftProperty putA

instance ArrowFix (p,x) (p,y) c => ArrowFix x y (PropertyArrow p c) where
  fixA f = PropertyArrow (StateArrow (fixA (runProperty . f . PropertyArrow . StateArrow)))

