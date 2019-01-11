{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Arrow.Transformer.Writer where

import Prelude hiding (id,(.),lookup,read,fail)

import Control.Category
import Control.Arrow
import Control.Arrow.Alloc
import Control.Arrow.Random
import Control.Arrow.Conditional as Cond
import Control.Arrow.State
import Control.Arrow.Reader
import Control.Arrow.Fail
import Control.Arrow.Lift
import Control.Arrow.Fix
import Control.Arrow.Except as Exc
import Control.Arrow.Store as Store
import Control.Arrow.Environment as Env
import Control.Arrow.Writer
import Control.Arrow.Abstract.Join

import Data.Monoidal
import Data.Order hiding (lub)

newtype WriterT w c x y = WriterT { runWriterT :: c x (w,y) }

instance Monoid w => ArrowLift (WriterT w) where
  lift f = WriterT (arr (const mempty) &&& f)

instance (Monoid w, Arrow c) => Category (WriterT w c) where
  id = WriterT (arr mempty &&& id)
  WriterT g . WriterT f = WriterT $ f >>> second g >>^ \(w1,(w2,z)) -> (w1 <> w2,z)
  -- proc x -> do
  --   (w1,y) <- f -< x
  --   (w2,z) <- g -< y
  --   returnA -< (w1 <> w2,z)

instance (Monoid w, Arrow c) => Arrow (WriterT w c) where
  arr f = WriterT (arr mempty &&& arr f)
  first (WriterT f) = WriterT (first f >>^ (\((w,b),d) -> (w,(b,d))))
  second (WriterT g) = WriterT (second g >>^ (\(d,(w,b)) -> (w,(d,b))))
  WriterT f *** WriterT g = WriterT (f *** g >>^ (\((w1,b),(w2,d)) -> (w1 <> w2,(b,d))))
  WriterT f &&& WriterT g = WriterT (f &&& g >>^ (\((w1,b),(w2,d)) -> (w1 <> w2,(b,d))))

instance (Monoid w, ArrowChoice c) => ArrowChoice (WriterT w c) where
  left (WriterT f) = WriterT (left f >>^ (\e -> case e of Left (w,x) -> (w,Left x); Right y -> (mempty,Right y)))
  right (WriterT f) = WriterT (right f >>^ (\e -> case e of Left x -> (mempty,Left x); Right (w,y) -> (w,Right y)))
  WriterT f ||| WriterT g = WriterT (f ||| g)
  WriterT f +++ WriterT g = WriterT (f +++ g >>^ from distribute)

instance (Monoid w, ArrowApply c) => ArrowApply (WriterT w c) where
  app = WriterT $ (\(WriterT f,x) -> (f,x)) ^>> app

instance (Monoid w, ArrowState s c) => ArrowState s (WriterT w c) where
  get = lift get
  put = lift put

instance (Monoid w, Arrow c) => ArrowWriter w (WriterT w c) where
  tell = WriterT (arr (\w -> (w,())))

instance (Monoid w, ArrowFail e c) => ArrowFail e (WriterT w c) where
  fail = lift fail

instance (Monoid w, ArrowExcept e c) => ArrowExcept e (WriterT w c) where
  type Join (WriterT w c) x y = Exc.Join c x (w,y)
  throw = lift throw
  catch (WriterT f) (WriterT g) = WriterT $ catch f g
  finally (WriterT f) (WriterT g) = WriterT $ finally f g

instance (Monoid w, ArrowReader r c) => ArrowReader r (WriterT w c) where
  ask = lift ask
  local (WriterT f) = WriterT (local f)

instance (Monoid w, ArrowEnv x y env c) => ArrowEnv x y env (WriterT w c) where
  type Join (WriterT w c) x y = Env.Join c x (w,y)
  lookup (WriterT f) (WriterT g) = WriterT $ lookup f g
  getEnv = lift getEnv
  extendEnv = lift extendEnv
  localEnv (WriterT f) = WriterT (localEnv f)

instance (Monoid w, ArrowStore var val c)  => ArrowStore var val (WriterT w c) where
  type Join (WriterT w c) x y = Store.Join c x (w,y)
  read (WriterT f) (WriterT g) = WriterT $ read f g
  write = lift write

type instance Fix x y (WriterT w c) = WriterT w (Fix x (w,y) c)
instance (Monoid w, ArrowFix x (w,y) c) => ArrowFix x y (WriterT w c) where
  fix f = WriterT (fix (runWriterT . f . WriterT))

instance (Monoid w, ArrowLoop c) => ArrowLoop (WriterT w c) where
  loop (WriterT f) = WriterT $ loop (f >>^ to assoc)

instance (Monoid w, Complete w, ArrowJoin c) => ArrowJoin (WriterT w c) where
  joinWith lub (WriterT f) (WriterT g) = WriterT $ joinWith (\(w1,z1) (w2,z2) -> (w1 ⊔ w2, lub z1 z2)) f g

instance (Monoid w, ArrowAlloc x y c) => ArrowAlloc x y (WriterT w c) where
  alloc = lift alloc

instance (Monoid w, ArrowCond v c) => ArrowCond v (WriterT w c) where
  type Join (WriterT w c) x y = Cond.Join c x (w,y)
  if_ (WriterT f) (WriterT g) = WriterT $ if_ f g

instance (Monoid w, ArrowRand v c) => ArrowRand v (WriterT w c) where
  random = lift random

deriving instance PreOrd (c x (w,y)) => PreOrd (WriterT w c x y)
deriving instance LowerBounded (c x (w,y)) => LowerBounded (WriterT w c x y)
deriving instance Complete (c x (w,y)) => Complete (WriterT w c x y)
deriving instance CoComplete (c x (w,y)) => CoComplete (WriterT w c x y)
deriving instance UpperBounded (c x (w,y)) => UpperBounded (WriterT w c x y)
