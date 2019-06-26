{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
module Control.Arrow.Transformer.Kleisli where

import           Prelude hiding (id,(.),lookup,read,fail)

import           Control.Arrow hiding (ArrowMonad)
import           Control.Arrow.Monad
import           Control.Arrow.Const
import           Control.Arrow.Deduplicate
import           Control.Arrow.Fix
import           Control.Arrow.State
import           Control.Arrow.Reader
import           Control.Arrow.Environment as Env
import           Control.Arrow.Store as Store
import           Control.Arrow.Except as Exc
import           Control.Arrow.Fail
import           Control.Arrow.Trans
import           Control.Category

import           Data.Identifiable
import           Data.Order
import           Data.Monoidal
import           Data.Profunctor (Profunctor(..))
import           Data.Coerce

newtype KleisliT f c x y = KleisliT { runKleisliT :: c x (f y) }

instance ArrowTrans (KleisliT f) where
  type Dom (KleisliT f) x y = x
  type Cod (KleisliT f) x y = f y
  lift = coerce
  {-# INLINE lift #-}
  unlift = coerce
  {-# INLINE unlift #-}

instance Monad f => ArrowLift (KleisliT f) where
  lift' f = lift $ rmap return f

instance ArrowMonad f c => Profunctor (KleisliT f c) where
  dimap f g h = lift $ dimap f (fmap g) $ unlift h
  lmap f h = lift $ lmap f (unlift h)
  rmap g h = lift $ rmap (fmap g) (unlift h)

instance ArrowMonad f c => Category (KleisliT f c) where
  id = lift unitA
  f . g = lift $ mapJoinA (unlift f) . unlift g

instance ArrowMonad f c => Arrow (KleisliT f c) where
  arr f = lift $ arr (return . f)
  first f = lift $ rmap strength1 (first (unlift f))
  second f = lift $ rmap strength2 (second (unlift f))
  f &&& g = lmap (\x -> (x,x)) (f *** g)
  f *** g = first f >>> second g

instance (ArrowMonad f c, ArrowChoice c) => ArrowChoice (KleisliT f c) where
  left f = lift $ rmap strength1 $ left (unlift f)
  right f = lift $ rmap strength2 $ right (unlift f)
  f ||| g = lift $ unlift f ||| unlift g
  f +++ g = left f >>> right g

instance (ArrowMonad f c, ArrowApply c) => ArrowApply (KleisliT f c) where
  app = lift $ lmap (first unlift) app

instance (ArrowMonad f c, ArrowState s c) => ArrowState s (KleisliT f c) where
  get = lift' get
  put = lift' put

instance (ArrowMonad f c, ArrowReader r c) => ArrowReader r (KleisliT f c) where
  ask = lift' ask
  local f = lift (local (unlift f))

instance (ArrowMonad f c, ArrowEnv x y env c) => ArrowEnv x y env (KleisliT f c) where
  type Join (KleisliT f c) x y = Env.Join c (Dom (KleisliT f) x y) (Cod (KleisliT f) x y)
  lookup f g = lift $ lookup (unlift f) (unlift g)
  getEnv = lift' getEnv
  extendEnv = lift' extendEnv
  localEnv f = lift (localEnv (unlift f))

instance (ArrowMonad f c, ArrowStore var val c) => ArrowStore var val (KleisliT f c) where
  type Join (KleisliT f c) x y = Store.Join c (Dom (KleisliT f) x y) (Cod (KleisliT f) x y)
  read f g = lift $ read (unlift f) (unlift g)
  write = lift' write

type instance Fix x y (KleisliT f c) = KleisliT f (Fix (Dom (KleisliT f) x y) (Cod (KleisliT f) x y) c)
instance (ArrowMonad f c, ArrowFix (Dom (KleisliT f) x y) (Cod (KleisliT f) x y) c) => ArrowFix x y (KleisliT f c) where
  fix = liftFix

instance (ArrowMonad f c, ArrowExcept e c) => ArrowExcept e (KleisliT f c) where
  type Join (KleisliT f c) (y,(x,e)) z = Exc.Join c (Cod (KleisliT f) x y,Dom (KleisliT f) (x,e) z) (Cod (KleisliT f) x z)
  throw = lift' throw
  try f g h = lift $ try (unlift f) (mapJoinA (unlift g)) (unlift h)

instance (ArrowMonad f c, ArrowFail e c) => ArrowFail e (KleisliT f c) where
  fail = lift' fail

instance (Identifiable (f y), ArrowMonad f c, Arrow c, ArrowDeduplicate (Dom (KleisliT f) x y) (Cod (KleisliT f) x y) c) => ArrowDeduplicate x y (KleisliT f c) where
  dedup f = lift (dedup (unlift f))

instance (ArrowMonad f c, ArrowConst r c) => ArrowConst r (KleisliT f c) where
  askConst f = lift (askConst (unlift . f))

deriving instance PreOrd (c x (f y)) => PreOrd (KleisliT f c x y)
deriving instance LowerBounded (c x (f y)) => LowerBounded (KleisliT f c x y)
deriving instance Complete (c x (f y)) => Complete (KleisliT f c x y)
deriving instance CoComplete (c x (f y)) => CoComplete (KleisliT f c x y)
deriving instance UpperBounded (c x (f y)) => UpperBounded (KleisliT f c x y)

