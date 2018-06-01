{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Transformer.State(State(..),evalState,execState) where

import Prelude hiding (id,(.),lookup,read)

import Control.Arrow
import Control.Arrow.Const
import Control.Arrow.Deduplicate
import Control.Arrow.Environment
import Control.Arrow.Fail
import Control.Arrow.Fix
import Control.Arrow.Lift
import Control.Arrow.Reader
import Control.Arrow.State
import Control.Arrow.Store
import Control.Arrow.Except
import Control.Arrow.Writer
import Control.Arrow.Utils

import Control.Arrow.Abstract.Join

import Control.Category

import Data.Hashable
import Data.Order hiding (lub)
import Data.Monoidal

-- Due to "Generalising Monads to Arrows", by John Hughes, in Science of Computer Programming 37.
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
  first (State f) = State $ (\(s,(b,c)) -> ((s,b),c)) ^>> first f >>^ (\((s,d),c) -> (s,(d,c)))
  second (State f) = State $ (\(s,(a,b)) -> (a,(s,b))) ^>> second f >>^ (\(a,(s,c)) -> (s,(a,c)))
  State f &&& State g = State $ (\(s,x) -> ((s,x),x)) ^>> first f >>> (\((s,y),x) -> ((s,x),y)) ^>> first g >>^ (\((s,z),y) -> (s,(y,z)))
  State f *** State g = State $ (\(s,(x,y)) -> ((s,x),y)) ^>> first f >>> (\((s,y),x) -> ((s,x),y)) ^>> first g >>^ (\((s,z),y) -> (s,(y,z)))

instance ArrowChoice c => ArrowChoice (State s c) where
  left (State f) = State (to distribute ^>> left f >>^ from distribute)
  right (State f) = State (to distribute ^>> right f >>^ from distribute)
  State f +++ State g = State $ to distribute ^>> f +++ g >>^ from distribute
  State f ||| State g = State $ to distribute ^>> f ||| g

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

instance ArrowWriter w c => ArrowWriter w (State s c) where
  tellA = lift tellA

instance ArrowEnv x y env c => ArrowEnv x y env (State s c) where
  lookup (State f) (State g) = State ((\(r,(env,a)) -> (env,(r,a))) ^>> lookup f g)
  getEnv = lift getEnv
  extendEnv = lift extendEnv
  localEnv (State f) = State ((\(r,(env,a)) -> (env,(r,a))) ^>> localEnv f)

instance ArrowStore var val lab c => ArrowStore var val lab (State s c) where
  read (State f) (State g) = State (read f g)
  write = lift write

type instance Fix x y (State s c) = State s (Fix (s,x) (s,y) c)
instance ArrowFix (s,x) (s,y) c => ArrowFix x y (State s c) where
  fixA f = State (fixA (runState . f . State))

instance ArrowExcept (s,x) (s,y) e c => ArrowExcept x y e (State s c) where
  tryCatchA (State f) (State g) = State $ tryCatchA f (from assoc ^>> g)
  finally (State f) (State g) = State $ finally f g

instance (Eq s, Hashable s, ArrowDeduplicate c) => ArrowDeduplicate (State s c) where
  dedupA (State f) = State (dedupA f)

instance ArrowLoop c => ArrowLoop (State s c) where
  loop (State f) = State $ loop ((\((s,b),d) -> (s,(b,d))) ^>> f >>^ (\(s,(b,d)) -> ((s,b),d)))

instance (ArrowJoin c, Complete s) => ArrowJoin (State s c) where
  joinWith lub (State f) (State g) =
    State $ (\(s,(x,y)) -> ((s,x),(s,y))) ^>> joinWith (\(s1,z1) (s2,z2) -> (s1⊔s2,lub z1 z2)) f g

instance ArrowConst r c => ArrowConst r (State s c) where
  askConst = lift askConst

deriving instance PreOrd (c (s,x) (s,y)) => PreOrd (State s c x y)
deriving instance LowerBounded (c (s,x) (s,y)) => LowerBounded (State s c x y)
deriving instance Complete (c (s,x) (s,y)) => Complete (State s c x y)
deriving instance CoComplete (c (s,x) (s,y)) => CoComplete (State s c x y)
deriving instance UpperBounded (c (s,x) (s,y)) => UpperBounded (State s c x y)
