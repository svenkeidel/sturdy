{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
module Control.Arrow.Transformer.State(StateArrow(..),liftState) where

import Prelude hiding (id,(.),lookup)

import Control.Arrow
import Control.Arrow.Class.Environment
import Control.Arrow.Class.Fail
import Control.Arrow.Class.Fix
import Control.Arrow.Class.Reader
import Control.Arrow.Class.State
import Control.Arrow.Deduplicate
import Control.Arrow.Try
import Control.Arrow.Utils
import Control.Category

import Data.Hashable
import Data.Order

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

instance ArrowApply c => ArrowApply (StateArrow s c) where
  app = StateArrow $ (\(s,(StateArrow f,b)) -> (f,(s,b))) ^>> app

instance Arrow c => ArrowState s (StateArrow s c) where
  getA = StateArrow (arr (\(a,()) -> (a,a)))
  putA = StateArrow (arr (\(_,s) -> (s,())))

instance ArrowFail e c => ArrowFail e (StateArrow s c) where
  failA = liftState failA

instance ArrowReader r c => ArrowReader r (StateArrow s c) where
  askA = liftState askA
  localA (StateArrow f) = StateArrow $ (\(s,(r,x)) -> (r,(s,x))) ^>> localA f

instance ArrowEnv x y env c => ArrowEnv x y env (StateArrow r c) where
  lookup = liftState lookup
  getEnv = liftState getEnv
  extendEnv = liftState extendEnv
  localEnv (StateArrow f) = StateArrow ((\(r,(env,a)) -> (env,(r,a))) ^>> localEnv f)

instance ArrowFix (s,x) (s,y) c => ArrowFix x y (StateArrow s c) where
  fixA f = StateArrow (fixA (runStateArrow . f . StateArrow))

instance ArrowTry c => ArrowTry (StateArrow s c) where
  tryA (StateArrow f) (StateArrow g) (StateArrow h) = StateArrow $ tryA f g h

instance ArrowZero c => ArrowZero (StateArrow s c) where
  zeroArrow = liftState zeroArrow

instance ArrowPlus c => ArrowPlus (StateArrow s c) where
  StateArrow f <+> StateArrow g = StateArrow (f <+> g)

instance (Eq s, Hashable s, ArrowDeduplicate c) => ArrowDeduplicate (StateArrow s c) where
  dedupA (StateArrow f) = StateArrow (dedupA f)

deriving instance PreOrd (c (s,x) (s,y)) => PreOrd (StateArrow s c x y)
deriving instance LowerBounded (c (s,x) (s,y)) => LowerBounded (StateArrow s c x y)
deriving instance Complete (c (s,x) (s,y)) => Complete (StateArrow s c x y)
deriving instance CoComplete (c (s,x) (s,y)) => CoComplete (StateArrow s c x y)
deriving instance UpperBounded (c (s,x) (s,y)) => UpperBounded (StateArrow s c x y)
