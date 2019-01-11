{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Transformer.Cont where

import Prelude hiding (id,(.),fail)

import Control.Category
import Control.Arrow
import Control.Arrow.Conditional as Cond
import Control.Arrow.Fix
import Control.Arrow.Fail
import Control.Arrow.Lift
import Control.Arrow.Reader
import Control.Arrow.State
import Control.Arrow.Writer

newtype ContT r c x y = ContT (c y r -> c x r)

runContT :: Category c => ContT y c x y -> c x y
runContT (ContT f) = f id

runContT' :: Category c => ContT r c x y -> c y r -> c x r
runContT' (ContT f) = f

instance Category (ContT r c) where
  id = ContT id
  ContT f . ContT g = ContT (g . f)

instance ArrowApply c => Arrow (ContT r c) where
  arr f = ContT $ \k -> k . arr f
  first (ContT f) = ContT $ \k -> proc (b,d) -> f (proc c -> k -< (c,d)) -<< b
  second (ContT f) = ContT $ \k -> proc (d,b) -> f (proc c -> k -< (d,c)) -<< b
  ContT f &&& ContT g = ContT $ \k -> proc b -> f (proc c1 -> g (proc c2 -> k -< (c1,c2)) -<< b) -<< b
  ContT f *** ContT g = ContT $ \k -> proc (b1,b2) -> f (proc c1 -> g (proc c2 -> k -< (c1,c2)) -<< b2) -<< b1

instance (ArrowApply c, ArrowChoice c) => ArrowChoice (ContT r c) where
  left (ContT f) = ContT $ \k -> f (k . arr Left) ||| (k . arr Right)
  right (ContT f) = ContT $ \k -> (k . arr Left) ||| f (k . arr Right)
  ContT f ||| ContT g = ContT $ \k -> f k ||| g k
  ContT f +++ ContT g = ContT $ \k -> f (k . arr Left) ||| g (k . arr Right)

-- instance (ArrowApply c) => ArrowFix x y (ContT r c) where
--   fix f = ContT $ \k -> fix (runCont' . f . ContT) k
--   fix f = ContT $ \k -> (f . ... . f) k

-- Instance that lifts the fixpoint operator through the continuation arrow.
instance (ArrowApply c, ArrowFix x y c) => ArrowFix x y (ContT y c) where
  fix f = ContT $ \k -> fix $ \h -> let ContT g = f (ContT (\j -> j . h)) in g k

instance ArrowLift (ContT r) where
  lift f = ContT $ \k -> k . f

instance (ArrowApply c, ArrowState s c) => ArrowState s (ContT r c) where
  get = lift get
  put = lift put

instance (ArrowApply c, ArrowReader s c) => ArrowReader s (ContT r c) where
  ask = lift ask
  local (ContT f) = ContT $ \k -> local (f k)

instance (ArrowApply c, ArrowWriter w c) => ArrowWriter w (ContT r c) where
  tell = lift tell

instance (ArrowApply c, ArrowFail e c) => ArrowFail e (ContT r c) where
  fail = lift fail

instance (ArrowApply c, ArrowCond v c) => ArrowCond v (ContT r c) where
  type Join (ContT r c) x y = Cond.Join c x r
  if_ (ContT f) (ContT g) = ContT $ \k -> if_ (f k) (g k)
