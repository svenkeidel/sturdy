{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Transformer.BackwardState where

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
import Data.Monoidal

newtype State s c x y = State { runState :: c (s,x) (s,y) }

evalBackwardState :: Arrow c => State s c x y -> c (s,x) y
evalBackwardState f = runState f >>> pi2

execBackwardState :: Arrow c => State s c x y -> c (s,x) s
execBackwardState f = runState f >>> pi1

instance ArrowLoop c => Category (State s c) where
  id = State id
  State f . State g = State $ proc (s1,x) -> do
    rec (s3,y) <- g -< (s2,x)
        (s2,z) <- f -< (s1,y)
    returnA -< (s3,z)

instance ArrowLift (State r) where
  lift f = State (second f)

instance ArrowLoop c => Arrow (State s c) where
  arr f = lift (arr f)
  first (State f) = State $ (\(s,(b,c)) -> ((s,b),c)) ^>> first f >>^ (\((s,d),c) -> (s,(d,c)))
  second (State f) = State $ (\(s,(a,b)) -> (a,(s,b))) ^>> second f >>^ (\(a,(s,c)) -> (s,(a,c)))
  State f &&& State g = State $ proc (s1,x) -> do
    rec (s3,y) <- f -< (s2,x)
        (s2,z) <- g -< (s1,x)
    returnA -< (s3,(y,z))
  State f *** State g = State $ proc (s1,(x,y)) -> do
    rec (s3,x') <- f -< (s2,x)
        (s2,y') <- g -< (s1,y)
    returnA -< (s3,(x',y'))

instance (ArrowLoop c, ArrowChoice c) => ArrowChoice (State s c) where
  left (State f) = State (to distribute ^>> left f >>^ from distribute)
  right (State f) = State (to distribute ^>> right f >>^ from distribute)
  State f +++ State g = State $ to distribute ^>> f +++ g >>^ from distribute
  State f ||| State g = State $ to distribute ^>> f ||| g

instance (ArrowLoop c, ArrowApply c) => ArrowApply (State s c) where
  app = State $ (\(s,(State f,b)) -> (f,(s,b))) ^>> app

instance (ArrowLoop c) => ArrowState s (State s c) where
  getA = State $ arr (\(s,_) -> (s,s))
  putA = State $ arr (\(_,s) -> (s,()))

instance (ArrowLoop c, ArrowFail e c) => ArrowFail e (State s c) where
  failA = lift failA

instance (ArrowLoop c, ArrowReader r c) => ArrowReader r (State s c) where
  askA = lift askA
  localA (State f) = State $ (\(s,(r,x)) -> (r,(s,x))) ^>> localA f

instance (ArrowLoop c, ArrowEnv x y env c) => ArrowEnv x y env (State r c) where
  lookup = lift lookup
  getEnv = lift getEnv
  extendEnv = lift extendEnv
  localEnv (State f) = State ((\(r,(env,a)) -> (env,(r,a))) ^>> localEnv f)
  getEnvDomain = lift getEnvDomain

instance (ArrowLoop c, ArrowStore var val lab c) => ArrowStore var val lab (State r c) where
  read = lift read
  write = lift write

type instance Fix x y (State s c) = State s (Fix (s,x) (s,y) c)
instance (ArrowLoop c, ArrowFix (s,x) (s,y) c) => ArrowFix x y (State s c) where
  fixA f = State (fixA (runState . f . State))

instance (ArrowLoop c, ArrowTry (s,x) (s,y) (s,z) c) => ArrowTry x y z (State s c) where
  tryA (State f) (State g) (State h) = State $ tryA f g h

instance (Eq s, Hashable s, ArrowLoop c, ArrowDeduplicate c) => ArrowDeduplicate (State s c) where
  dedupA (State f) = State (dedupA f)

deriving instance PreOrd (c (s,x) (s,y)) => PreOrd (State s c x y)
deriving instance LowerBounded (c (s,x) (s,y)) => LowerBounded (State s c x y)
deriving instance Complete (c (s,x) (s,y)) => Complete (State s c x y)
deriving instance CoComplete (c (s,x) (s,y)) => CoComplete (State s c x y)
deriving instance UpperBounded (c (s,x) (s,y)) => UpperBounded (State s c x y)
