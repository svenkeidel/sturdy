{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Transformer.State where

import Prelude hiding (id,(.))
import Control.Category
import Control.Arrow
import Control.Arrow.Class.Fail
import Control.Arrow.Class.State
import Control.Arrow.Class.Reader
import Control.Arrow.Utils

newtype StateArrow s c x y = StateArrow { runStateArrow :: c (s,x) (s,y) }

instance Category c => Category (StateArrow s c) where
  id = StateArrow id
  StateArrow f . StateArrow g = StateArrow (f . g)

liftState :: Arrow c => c x y -> StateArrow s c x y
liftState f = StateArrow (second f)
{-# INLINE liftState #-}

instance Arrow c => Arrow (StateArrow s c) where
  arr f = liftState (arr f)
  first (StateArrow f) = StateArrow $ (\(s,(x,y)) -> ((s,x),y)) ^>> first f >>^ (\((s',x'),y) -> (s',(x',y)))
  second (StateArrow f) = StateArrow $ (\(s,(x,y)) -> (x,(s,y))) ^>> second f >>^ (\(x,(s',y')) -> (s',(x,y')))

instance ArrowChoice c => ArrowChoice (StateArrow s c) where
  left (StateArrow f) = StateArrow $ injectBoth ^>> left f >>^ eject
  right (StateArrow f) = StateArrow $ injectBoth ^>> right f >>^ eject

instance Arrow c => ArrowState s (StateArrow s c) where
  getA = StateArrow (arr (\(a,()) -> (a,a)))
  putA = StateArrow (arr (\(_,s) -> (s,())))

instance ArrowFail e c => ArrowFail e (StateArrow s c) where
  failA = liftState failA

instance ArrowReader r c => ArrowReader r (StateArrow s c) where
  askA = liftState askA
  localA (StateArrow f) = StateArrow $ (\(s,(r,x)) -> (r,(s,x))) ^>> localA f
