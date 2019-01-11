{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
module Control.Arrow.Transformer.State(StateT(..),evalStateT,execStateT) where

import Prelude hiding (id,(.),lookup,read,fail)

import Control.Arrow
import Control.Arrow.Alloc
import Control.Arrow.Const
import Control.Arrow.Conditional as Cond
import Control.Arrow.Deduplicate
import Control.Arrow.Environment as Env
import Control.Arrow.Fail
import Control.Arrow.Fix
import Control.Arrow.Lift
import Control.Arrow.Random
import Control.Arrow.Reader
import Control.Arrow.State
import Control.Arrow.Store as Store
import Control.Arrow.Except as Exc
import Control.Arrow.Writer
import Control.Arrow.Utils

import Control.Arrow.Abstract.Join

import Control.Category

import Data.Hashable
import Data.Order hiding (lub)
import Data.Monoidal

-- Due to "Generalising Monads to Arrows", by John Hughes, in Science of Computer Programming 37.
newtype StateT s c x y = StateT { runStateT :: c (s,x) (s,y) }

evalStateT :: Arrow c => StateT s c x y -> c (s,x) y
evalStateT f = runStateT f >>> pi2

execStateT :: Arrow c => StateT s c x y -> c (s,x) s
execStateT f = runStateT f >>> pi1

instance Category c => Category (StateT s c) where
  id = StateT id
  StateT f . StateT g = StateT (f . g)

instance ArrowLift (StateT r) where
  lift f = StateT (second f)

instance Arrow c => Arrow (StateT s c) where
  arr f = lift (arr f)
  first (StateT f) = StateT $ (\(s,(b,c)) -> ((s,b),c)) ^>> first f >>^ (\((s,d),c) -> (s,(d,c)))
  second (StateT f) = StateT $ (\(s,(a,b)) -> (a,(s,b))) ^>> second f >>^ (\(a,(s,c)) -> (s,(a,c)))
  StateT f &&& StateT g = StateT $ (\(s,x) -> ((s,x),x)) ^>> first f >>> (\((s,y),x) -> ((s,x),y)) ^>> first g >>^ (\((s,z),y) -> (s,(y,z)))
  StateT f *** StateT g = StateT $ (\(s,(x,y)) -> ((s,x),y)) ^>> first f >>> (\((s,y),x) -> ((s,x),y)) ^>> first g >>^ (\((s,z),y) -> (s,(y,z)))

instance ArrowChoice c => ArrowChoice (StateT s c) where
  left (StateT f) = StateT (to distribute ^>> left f >>^ from distribute)
  right (StateT f) = StateT (to distribute ^>> right f >>^ from distribute)
  StateT f +++ StateT g = StateT $ to distribute ^>> f +++ g >>^ from distribute
  StateT f ||| StateT g = StateT $ to distribute ^>> f ||| g

instance ArrowApply c => ArrowApply (StateT s c) where
  app = StateT $ (\(s,(StateT f,b)) -> (f,(s,b))) ^>> app

instance Arrow c => ArrowState s (StateT s c) where
  get = StateT (arr (\(a,()) -> (a,a)))
  put = StateT (arr (\(_,s) -> (s,())))

instance ArrowFail e c => ArrowFail e (StateT s c) where
  fail = lift fail

instance ArrowReader r c => ArrowReader r (StateT s c) where
  ask = lift ask
  local (StateT f) = StateT $ (\(s,(r,x)) -> (r,(s,x))) ^>> local f

instance ArrowWriter w c => ArrowWriter w (StateT s c) where
  tell = lift tell

instance (ArrowEnv var val env c) => ArrowEnv var val env (StateT s c) where
  type instance Join (StateT s c) ((val,x),x) y = Env.Join c ((val,(s,x)),(s,x)) (s,y)
  lookup (StateT f) (StateT g) = StateT $ (\(s,(v,a)) -> (v,(s,a))) ^>> lookup ((\(v,(s,a)) -> (s,(v,a))) ^>> f) g
  getEnv = lift getEnv
  extendEnv = lift extendEnv
  localEnv (StateT f) = StateT ((\(r,(env,a)) -> (env,(r,a))) ^>> localEnv f)

instance (ArrowStore var val c) => ArrowStore var val (StateT s c) where
  type instance Join (StateT s c) ((val,x),x) y = Store.Join c ((val,(s,x)),(s,x)) (s,y)
  read (StateT f) (StateT g) = StateT $ (\(s,(v,a)) -> (v,(s,a))) ^>> read ((\(v,(s,a)) -> (s,(v,a))) ^>> f) g
  write = lift write

type instance Fix x y (StateT s c) = StateT s (Fix (s,x) (s,y) c)
instance ArrowFix (s,x) (s,y) c => ArrowFix x y (StateT s c) where
  fix f = StateT (fix (runStateT . f . StateT))

instance (ArrowExcept e c) => ArrowExcept e (StateT s c) where
  type instance Join (StateT s c) (x,(x,e)) y = Exc.Join c ((s,x),((s,x),e)) (s,y)
  throw = lift throw
  catch (StateT f) (StateT g) = StateT $ catch f (from assoc ^>> g)
  finally (StateT f) (StateT g) = StateT $ finally f g

instance (Eq s, Hashable s, ArrowDeduplicate (s, x) (s, y) c) => ArrowDeduplicate x y (StateT s c) where
  dedup (StateT f) = StateT (dedup f)

instance ArrowLoop c => ArrowLoop (StateT s c) where
  loop (StateT f) = StateT $ loop ((\((s,b),d) -> (s,(b,d))) ^>> f >>^ (\(s,(b,d)) -> ((s,b),d)))

instance (ArrowJoin c, Complete s) => ArrowJoin (StateT s c) where
  joinWith lub (StateT f) (StateT g) =
    StateT $ (\(s,(x,y)) -> ((s,x),(s,y))) ^>> joinWith (\(s1,z1) (s2,z2) -> (s1⊔s2,lub z1 z2)) f g

instance ArrowConst x c => ArrowConst x (StateT s c) where
  askConst = lift askConst

instance ArrowAlloc x y c => ArrowAlloc x y (StateT s c) where
  alloc = lift alloc

instance (ArrowCond v c) => ArrowCond v (StateT s c) where
  type instance Join (StateT s c) (x,y) z = Cond.Join c ((s,x),(s,y)) (s,z)
  if_ (StateT f) (StateT g) = StateT $ (\(s,(v,(x,y))) -> (v,((s,x),(s,y)))) ^>> if_ f g

instance ArrowRand v c => ArrowRand v (StateT s c) where
  random = lift random

deriving instance PreOrd (c (s,x) (s,y)) => PreOrd (StateT s c x y)
deriving instance LowerBounded (c (s,x) (s,y)) => LowerBounded (StateT s c x y)
deriving instance Complete (c (s,x) (s,y)) => Complete (StateT s c x y)
deriving instance CoComplete (c (s,x) (s,y)) => CoComplete (StateT s c x y)
deriving instance UpperBounded (c (s,x) (s,y)) => UpperBounded (StateT s c x y)
