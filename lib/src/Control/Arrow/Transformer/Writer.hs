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
import Control.Arrow.Trans
import Control.Arrow.Fix
import Control.Arrow.Except as Exc
import Control.Arrow.Store as Store
import Control.Arrow.Environment as Env
import Control.Arrow.Writer
import Control.Arrow.Abstract.Join

import Data.Monoidal
import Data.Order hiding (lub)

newtype WriterT w c x y = WriterT { runWriterT :: c x (w,y) }

instance ArrowTrans (WriterT w) where
  type Dom (WriterT w) x y = x
  type Cod (WriterT w) x y = (w,y)
  lift = WriterT
  unlift = runWriterT

instance Monoid w => ArrowLift (WriterT w) where
  lift' f = lift (arr (const mempty) &&& f)

instance (Monoid w, Arrow c) => Category (WriterT w c) where
  id = lift (arr mempty &&& id)
  g . f = lift $ unlift f >>> second (unlift g) >>^ \(w1,(w2,z)) -> (w1 <> w2,z)
  -- proc x -> do
  --   (w1,y) <- f -< x
  --   (w2,z) <- g -< y
  --   returnA -< (w1 <> w2,z)

instance (Monoid w, Arrow c) => Arrow (WriterT w c) where
  arr f = lift (arr mempty &&& arr f)
  first f = lift (first (unlift f) >>^ (\((w,b),d) -> (w,(b,d))))
  second g = lift (second (unlift g) >>^ (\(d,(w,b)) -> (w,(d,b))))
  f *** g = lift (unlift f *** unlift g >>^ (\((w1,b),(w2,d)) -> (w1 <> w2,(b,d))))
  f &&& g = lift (unlift f &&& unlift g >>^ (\((w1,b),(w2,d)) -> (w1 <> w2,(b,d))))

instance (Monoid w, ArrowChoice c) => ArrowChoice (WriterT w c) where
  left f = lift (left (unlift f) >>^ (\e -> case e of Left (w,x) -> (w,Left x); Right y -> (mempty,Right y)))
  right f = lift (right (unlift f) >>^ (\e -> case e of Left x -> (mempty,Left x); Right (w,y) -> (w,Right y)))
  f ||| g = lift (unlift f ||| unlift g)
  f +++ g = lift (unlift f +++ unlift g >>^ from distribute)

instance (Monoid w, ArrowApply c) => ArrowApply (WriterT w c) where
  app = lift $ (\(f,x) -> (unlift f,x)) ^>> app

instance (Monoid w, ArrowState s c) => ArrowState s (WriterT w c) where
  get = lift' get
  put = lift' put

instance (Monoid w, Arrow c) => ArrowWriter w (WriterT w c) where
  tell = lift (arr (\w -> (w,())))

instance (Monoid w, ArrowFail e c) => ArrowFail e (WriterT w c) where
  fail = lift' fail

instance (Monoid w, ArrowExcept e c) => ArrowExcept e (WriterT w c) where
  type Join (WriterT w c) x y = Exc.Join c (Dom (WriterT w) x y) (Cod (WriterT w) x y)
  throw = lift' throw
  catch f g = lift $ catch (unlift f) (unlift g)
  finally f g = lift $ finally (unlift f) (unlift g)

instance (Monoid w, ArrowReader r c) => ArrowReader r (WriterT w c) where
  ask = lift' ask
  local f = lift (local (unlift f))

instance (Monoid w, ArrowEnv x y env c) => ArrowEnv x y env (WriterT w c) where
  type Join (WriterT w c) x y = Env.Join c (Dom (WriterT w) x y) (Cod (WriterT w) x y)
  lookup f g = lift $ lookup (unlift f) (unlift g)
  getEnv = lift' getEnv
  extendEnv = lift' extendEnv
  localEnv f = lift (localEnv (unlift f))

instance (Monoid w, ArrowStore var val c) => ArrowStore var val (WriterT w c) where
  type Join (WriterT w c) x y = Store.Join c (Dom (WriterT w) x y) (Cod (WriterT w) x y)
  read f g = lift $ read (unlift f) (unlift g)
  write = lift' write

type instance Fix x y (WriterT w c) = WriterT w (Fix (Dom (WriterT w) x y) (Cod (WriterT w) x y) c)
instance (Monoid w, ArrowFix x (w,y) c) => ArrowFix x y (WriterT w c) where
  fix = liftFix

instance (Monoid w, Complete w, ArrowJoin c) => ArrowJoin (WriterT w c) where
  joinWith lub f g = lift $ joinWith (\(w1,z1) (w2,z2) -> (w1 ⊔ w2, lub z1 z2)) (unlift f) (unlift g)

instance (Monoid w, ArrowAlloc x y c) => ArrowAlloc x y (WriterT w c) where
  alloc = lift' alloc

instance (Monoid w, ArrowCond v c) => ArrowCond v (WriterT w c) where
  type Join (WriterT w c) x y = Cond.Join c (Dom (WriterT w) x y) (Cod (WriterT w) x y)
  if_ f g = lift $ if_ (unlift f) (unlift g)

instance (Monoid w, ArrowRand v c) => ArrowRand v (WriterT w c) where
  random = lift' random

deriving instance PreOrd (c x (w,y)) => PreOrd (WriterT w c x y)
deriving instance LowerBounded (c x (w,y)) => LowerBounded (WriterT w c x y)
deriving instance Complete (c x (w,y)) => Complete (WriterT w c x y)
deriving instance CoComplete (c x (w,y)) => CoComplete (WriterT w c x y)
deriving instance UpperBounded (c x (w,y)) => UpperBounded (WriterT w c x y)
