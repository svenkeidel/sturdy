{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Control.Arrow.Transformer.Reader(ReaderT(..)) where

import Prelude hiding (id,(.),lookup,read,fail)

import Control.Arrow
import Control.Arrow.Const
import Control.Arrow.Conditional as Cond
import Control.Arrow.Environment as Env
import Control.Arrow.Fail
import Control.Arrow.Fix
import Control.Arrow.Reader
import Control.Arrow.Store as Store
import Control.Arrow.State
import Control.Arrow.Deduplicate
import Control.Arrow.Except as Exc
import Control.Arrow.Lift
import Control.Arrow.Writer
import Control.Arrow.Utils
import Control.Arrow.Abstract.Join
import Control.Category

import Data.Order hiding (lub)
import Data.Monoidal

-- Due to "Generalising Monads to Arrows", by John Hughes, in Science of Computer Programming 37.
newtype ReaderT r c x y = ReaderT { runReaderT :: c (r,x) y }

instance ArrowLift (ReaderT r) where
  lift f = ReaderT (pi2 >>> f)

instance Arrow c => Category (ReaderT r c) where
  id = lift id
  ReaderT f . ReaderT g = ReaderT $ (\(r,x) -> (r,(r,x))) ^>> f . second g

instance Arrow c => Arrow (ReaderT r c) where
  arr f = lift (arr f)
  first (ReaderT f) = ReaderT $ (\(r,(b,d)) -> ((r,b),d)) ^>> first f
  second (ReaderT f) = ReaderT $ (\(r,(b,d)) -> (b,(r,d))) ^>> second f
  ReaderT f &&& ReaderT g = ReaderT $ f &&& g
  ReaderT f *** ReaderT g = ReaderT $ (\(r,(b,d)) -> ((r,b),(r,d))) ^>> f *** g

instance ArrowChoice c => ArrowChoice (ReaderT r c) where
  left (ReaderT f) = ReaderT $ to distribute ^>> mmap id pi2  ^>> left f
  right (ReaderT f) = ReaderT $ to distribute ^>> mmap pi2 id ^>> right f
  ReaderT f +++ ReaderT g = ReaderT (to distribute ^>> f +++ g)
  ReaderT f ||| ReaderT g = ReaderT (to distribute ^>> f ||| g)

instance ArrowApply c => ArrowApply (ReaderT r c) where
  app = ReaderT $ (\(r,(ReaderT f,b)) -> (f,(r,b))) ^>> app

instance Arrow c => ArrowReader r (ReaderT r c) where
  ask = ReaderT pi1
  local (ReaderT f) = ReaderT $ (\(_,(r,x)) -> (r,x)) ^>> f

instance ArrowState s c => ArrowState s (ReaderT r c) where
  get = lift get
  put = lift put

instance ArrowWriter w c => ArrowWriter w (ReaderT r c) where
  tell = lift tell

instance ArrowFail e c => ArrowFail e (ReaderT r c) where
  fail = lift fail

instance ArrowEnv var val env c => ArrowEnv var val env (ReaderT r c) where
  type instance Join (ReaderT r c) ((val,x),x) y = Env.Join c ((val,(r,x)),(r,x)) y
  lookup (ReaderT f) (ReaderT g) = ReaderT $ (\(r,(v,a)) -> (v,(r,a))) ^>> lookup ((\(v,(r,a)) -> (r,(v,a))) ^>> f) g
  getEnv = lift getEnv
  extendEnv = lift extendEnv
  localEnv (ReaderT f) = ReaderT ((\(r,(env,a)) -> (env,(r,a))) ^>> localEnv f)

instance ArrowStore var val c => ArrowStore var val (ReaderT r c) where
  type instance Join (ReaderT r c) ((val,x),x) y = Store.Join c ((val,(r,x)),(r,x)) y
  read (ReaderT f) (ReaderT g) = ReaderT $ (\(r,(v,a)) -> (v,(r,a))) ^>> read ((\(v,(r,a)) -> (r,(v,a))) ^>> f) g
  write = lift write

type instance Fix x y (ReaderT r c) = ReaderT r (Fix (r,x) y c)
instance ArrowFix (r,x) y c => ArrowFix x y (ReaderT r c) where
  fix f = ReaderT (fix (runReaderT . f . ReaderT))

instance ArrowExcept e c => ArrowExcept e (ReaderT r c) where
  type instance Join (ReaderT r c) (x,(x,e)) y = Exc.Join c ((r,x),((r,x),e)) y
  throw = lift throw
  catch (ReaderT f) (ReaderT g) = ReaderT $ catch f (from assoc ^>> g)
  finally (ReaderT f) (ReaderT g) = ReaderT $ finally f g

instance ArrowDeduplicate (r, x) y c => ArrowDeduplicate x y (ReaderT r c) where
  dedup (ReaderT f) = ReaderT (dedup f)

instance ArrowJoin c => ArrowJoin (ReaderT r c) where
  joinWith lub (ReaderT f) (ReaderT g) = ReaderT $ (\(r,(x,y)) -> ((r,x),(r,y))) ^>> joinWith lub f g

instance ArrowConst x c => ArrowConst x (ReaderT r c) where
  askConst = lift askConst

instance ArrowCond v c => ArrowCond v (ReaderT r c) where
  type instance Join (ReaderT r c) (x,y) z = Cond.Join c ((r,x),(r,y)) z
  if_ (ReaderT f) (ReaderT g) = ReaderT $ (\(r,(v,(x,y))) -> (v,((r,x),(r,y)))) ^>> if_ f g

deriving instance PreOrd (c (r,x) y) => PreOrd (ReaderT r c x y)
deriving instance LowerBounded (c (r,x) y) => LowerBounded (ReaderT r c x y)
deriving instance Complete (c (r,x) y) => Complete (ReaderT r c x y)
deriving instance CoComplete (c (r,x) y) => CoComplete (ReaderT r c x y)
deriving instance UpperBounded (c (r,x) y) => UpperBounded (ReaderT r c x y)
