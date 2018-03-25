{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
module Control.Arrow.Transformer.State(State(..),evalState,execState) where

import Prelude hiding (id,(.),lookup,read)

import Control.Arrow
import Control.Arrow.Deduplicate
import Control.Arrow.Environment
import Control.Arrow.Fail
import Control.Arrow.Fix
import Control.Arrow.Lift
import Control.Arrow.Reader
import Control.Arrow.State
import Control.Arrow.Store
import Control.Arrow.Try
import Control.Arrow.Utils
import Control.Category

import Data.Hashable
import Data.Order

newtype State s c x y = State { runState :: c (s,x) (s,y) }

evalState :: Arrow c => State s c x y -> c (s,x) y
evalState f = runState f >>> pi2

execState :: Arrow c => State s c x y -> c (s,x) s
execState f = runState f >>> pi1

instance Category c => Category (State s c) where
  id = State id
  State f . State g = State (f . g)

instance ArrowLift (State r) where
  lift f = State (second f)

instance Arrow c => Arrow (State s c) where
  arr f = lift (arr f)
  first (State f) = State $ (\(s,(x,y)) -> ((s,x),y)) ^>> first f >>^ (\((s',x'),y) -> (s',(x',y)))
  second (State f) = State $ (\(s,(x,y)) -> (x,(s,y))) ^>> second f >>^ (\(x,(s',y')) -> (s',(x,y')))

instance ArrowChoice c => ArrowChoice (State s c) where
  left (State f) = State $ injectBoth ^>> left f >>^ eject
  right (State f) = State $ injectBoth ^>> right f >>^ eject

instance ArrowApply c => ArrowApply (State s c) where
  app = State $ (\(s,(State f,b)) -> (f,(s,b))) ^>> app

instance Arrow c => ArrowState s (State s c) where
  getA = State (arr (\(a,()) -> (a,a)))
  putA = State (arr (\(_,s) -> (s,())))

instance ArrowFail e c => ArrowFail e (State s c) where
  failA = lift failA

instance ArrowReader r c => ArrowReader r (State s c) where
  askA = lift askA
  localA (State f) = State $ (\(s,(r,x)) -> (r,(s,x))) ^>> localA f

instance ArrowEnv x y env c => ArrowEnv x y env (State r c) where
  lookup = lift lookup
  getEnv = lift getEnv
  extendEnv = lift extendEnv
  localEnv (State f) = State ((\(r,(env,a)) -> (env,(r,a))) ^>> localEnv f)

instance ArrowStore var val c => ArrowStore var val (State r c) where
  read = lift read
  write = lift write

instance ArrowFix (s,x) (s,y) c => ArrowFix x y (State s c) where
  fixA f = State (fixA (runState . f . State))

instance ArrowTry (s,x) (s,y) (s,z) c => ArrowTry x y z (State s c) where
  tryA (State f) (State g) (State h) = State $ tryA f g h

instance ArrowZero c => ArrowZero (State s c) where
  zeroArrow = lift zeroArrow

instance ArrowPlus c => ArrowPlus (State s c) where
  State f <+> State g = State (f <+> g)

instance (Eq s, Hashable s, ArrowDeduplicate c) => ArrowDeduplicate (State s c) where
  dedupA (State f) = State (dedupA f)

deriving instance PreOrd (c (s,x) (s,y)) => PreOrd (State s c x y)
deriving instance LowerBounded (c (s,x) (s,y)) => LowerBounded (State s c x y)
deriving instance Complete (c (s,x) (s,y)) => Complete (State s c x y)
deriving instance CoComplete (c (s,x) (s,y)) => CoComplete (State s c x y)
deriving instance UpperBounded (c (s,x) (s,y)) => UpperBounded (State s c x y)
