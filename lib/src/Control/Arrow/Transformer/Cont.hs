{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
module Control.Arrow.Transformer.Cont where

import Prelude hiding (id,(.),fail)

import Control.Category
import Control.Arrow
import Control.Arrow.Fix
import Control.Arrow.Fail
import Control.Arrow.Trans
import Control.Arrow.Reader
import Control.Arrow.State
import Control.Arrow.Writer

newtype ContT c x y = ContT { runContT :: forall r. c y r -> c x r }

instance Category (ContT c) where
  id = ContT id
  ContT f . ContT g = ContT (g . f)

instance ArrowApply c => Arrow (ContT c) where
  arr f = ContT $ \k -> k . arr f
  first (ContT f) = ContT $ \k -> proc (b,d) -> f (proc c -> k -< (c,d)) -<< b
  second (ContT f) = ContT $ \k -> proc (d,b) -> f (proc c -> k -< (d,c)) -<< b
  ContT f &&& ContT g = ContT $ \k -> proc b -> f (proc c1 -> g (proc c2 -> k -< (c1,c2)) -<< b) -<< b
  ContT f *** ContT g = ContT $ \k -> proc (b1,b2) -> f (proc c1 -> g (proc c2 -> k -< (c1,c2)) -<< b2) -<< b1

instance (ArrowApply c, ArrowChoice c) => ArrowChoice (ContT c) where
  left (ContT f) = ContT $ \k -> f (k . arr Left) ||| (k . arr Right)
  right (ContT f) = ContT $ \k -> (k . arr Left) ||| f (k . arr Right)
  ContT f ||| ContT g = ContT $ \k -> f k ||| g k
  ContT f +++ ContT g = ContT $ \k -> f (k . arr Left) ||| g (k . arr Right)

type instance Fix x y (ContT c) = ContT (Fix (Dom ContT x y) (Cod ContT x y) c)
instance (ArrowApply c, ArrowFix x y c) => ArrowFix x y (ContT c) where
  fix = liftFix

-- | Lift and unlift proof the yoneda lemma.
instance ArrowTrans ContT where
  type Dom ContT x y = x
  type Cod ContT x y = y
  lift f = ContT $ \k -> k . f
  unlift (ContT f) = f id

instance ArrowLift ContT where
  lift' f = ContT $ \k -> k . f

instance (ArrowApply c, ArrowState s c) => ArrowState s (ContT c) where
  get = lift' get
  put = lift' put

instance (ArrowApply c, ArrowReader s c) => ArrowReader s (ContT c) where
  ask = lift' ask
  local (ContT f) = ContT $ \k -> local (f k)

instance (ArrowApply c, ArrowWriter w c) => ArrowWriter w (ContT c) where
  tell = lift' tell

instance (ArrowApply c, ArrowFail e c) => ArrowFail e (ContT c) where
  fail = lift' fail

-- instance (ArrowApply c, ArrowCond v c) => ArrowCond v (ContT c) where
--   type Join (ContT c) x y = Cond.Join c (Dom1 ContT x y) (Cod1 ContT x y)
--   if_ (ContT f) (ContT g) = ContT $ \k -> if_ (f k) (g k)
