{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE GADTs #-}
module Control.Arrow.Transformer.Abstract.Error(ErrorT(..)) where

import Prelude hiding (id,lookup,(.),read,fail)

import Control.Arrow
import Control.Arrow.Const
import Control.Arrow.Deduplicate
import Control.Arrow.Environment as Env
import Control.Arrow.Fail
import Control.Arrow.Trans
import Control.Arrow.Reader
import Control.Arrow.State
import Control.Arrow.Store as Store
import Control.Arrow.Except as Exc
import Control.Arrow.Fix
import Control.Arrow.Utils (duplicate)
import Control.Arrow.Abstract.Join
import Control.Category

import Data.Monoidal
import Data.Order
import Data.Profunctor
import Data.Abstract.Error
import Data.Abstract.Widening (toJoin2)

newtype ErrorT e c x y = ErrorT { runErrorT :: c x (Error e y)}

instance (ArrowChoice c, Profunctor c) => ArrowFail e (ErrorT e c) where
  fail = lift $ arr Fail

instance (ArrowChoice c, Profunctor c) => Category (ErrorT e c) where
  id = lift' id
  f . g = lift $ rmap toEither (unlift g) >>> (arr Fail ||| unlift f)

instance (Profunctor c, Arrow c) => Profunctor (ErrorT e c) where
  dimap f g h = lift $ dimap f (fmap g) (unlift h)
  lmap f h = lift $ lmap f (unlift h)
  rmap g h = lift $ rmap (fmap g) (unlift h)

instance ArrowLift (ErrorT e) where
  lift' f = ErrorT (rmap Success f)

instance ArrowTrans (ErrorT e) where
  type Dom (ErrorT e) x y = x
  type Cod (ErrorT e) x y = Error e y
  lift = ErrorT
  unlift = runErrorT

instance (ArrowChoice c, Profunctor c) => Arrow (ErrorT e c) where
  arr f    = lift' (arr f)
  first f  = lift $ rmap strength1 (first (unlift f))
  second f = lift $ rmap strength2 (second (unlift f))
  f &&& g = lmap duplicate (f *** g)
  f *** g = first f >>> second g

instance (ArrowChoice c, Profunctor c) => ArrowChoice (ErrorT e c) where
  left f  = lift $ rmap strength1 (left (unlift f))
  right f = lift $ rmap strength2 (right (unlift f))
  f ||| g = lift $ unlift f ||| unlift g
  f +++ g = left f >>> right g

instance (ArrowApply c, ArrowChoice c, Profunctor c) => ArrowApply (ErrorT e c) where
  app = lift $ lmap (first unlift) app

instance (ArrowState s c, ArrowChoice c, Profunctor c) => ArrowState s (ErrorT e c) where
  get = lift' get
  put = lift' put

instance (ArrowStore var val c, ArrowChoice c, Profunctor c) => ArrowStore var val (ErrorT e c) where
  type Join (ErrorT e c) x y = Store.Join c (Dom (ErrorT e) x y) (Cod (ErrorT e) x y)
  read f g = lift $ read (unlift f) (unlift g)
  write = lift' write

instance (ArrowReader r c, ArrowChoice c, Profunctor c) => ArrowReader r (ErrorT e c) where
  ask = lift' ask
  local f = lift (local (unlift f))

instance (ArrowEnv x y env c, ArrowChoice c, Profunctor c) => ArrowEnv x y env (ErrorT e c) where
  type Join (ErrorT e c) x y = Env.Join c (Dom (ErrorT e) x y) (Cod (ErrorT e) x y)
  lookup f g = lift $ lookup (unlift f) (unlift g)
  getEnv = lift' getEnv
  extendEnv = lift' extendEnv
  localEnv f = lift (localEnv (unlift f))

instance (ArrowChoice c, ArrowExcept e c) => ArrowExcept e (ErrorT e' c) where
  type Join (ErrorT e' c) x y = Exc.Join c (Dom (ErrorT e') x y) (Cod (ErrorT e') x y)
  throw = lift $ throw
  catch f g = lift $ catch (unlift f) (unlift g)
  finally f g = lift $ finally (unlift f) (unlift g)

type instance Fix x y (ErrorT e c) = ErrorT e (Fix (Dom (ErrorT e) x y) (Cod (ErrorT e) x y) c)
instance (ArrowFix (Dom (ErrorT e) x y) (Cod (ErrorT e) x y) c, ArrowChoice c, Profunctor c) => ArrowFix x y (ErrorT e c) where
  fix = liftFix

instance (ArrowDeduplicate (Dom (ErrorT e) x y) (Cod (ErrorT e) x y) c, ArrowChoice c, Profunctor c) => ArrowDeduplicate x y (ErrorT e c) where
  dedup f = lift $ dedup (unlift f)

instance (ArrowConst r c, ArrowChoice c) => ArrowConst r (ErrorT e c) where
  askConst = lift' askConst

instance (Complete e, ArrowJoin c, ArrowChoice c) => ArrowJoin (ErrorT e c) where
  joinWith lub' f g = ErrorT $ joinWith (toJoin2 widening (⊔) lub') (unlift f) (unlift g)

deriving instance PreOrd (c x (Error e y)) => PreOrd (ErrorT e c x y)
deriving instance LowerBounded (c x (Error e y)) => LowerBounded (ErrorT e c x y)
deriving instance Complete (c x (Error e y)) => Complete (ErrorT e c x y)
deriving instance CoComplete (c x (Error e y)) => CoComplete (ErrorT e c x y)
deriving instance UpperBounded (c x (Error e y)) => UpperBounded (ErrorT e c x y)
