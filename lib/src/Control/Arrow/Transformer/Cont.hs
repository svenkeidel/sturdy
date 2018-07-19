{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Transformer.Cont where

import Prelude hiding (id,(.),fail)

import Control.Category
import Control.Arrow
import Control.Arrow.Conditional
import Control.Arrow.Fix
import Control.Arrow.Fail
import Control.Arrow.Lift
import Control.Arrow.Reader
import Control.Arrow.State
import Control.Arrow.Writer

newtype Cont r c x y = Cont {runCont :: c y r -> c x r}

instance Category (Cont r c) where
  id = Cont id
  Cont f . Cont g = Cont (g . f)

instance ArrowApply c => Arrow (Cont r c) where
  arr f = Cont $ \k -> k . arr f
  first (Cont f) = Cont $ \k -> proc (b,d) -> f (proc c -> k -< (c,d)) -<< b
  second (Cont f) = Cont $ \k -> proc (d,b) -> f (proc c -> k -< (d,c)) -<< b
  Cont f &&& Cont g = Cont $ \k -> proc b -> f (proc c1 -> g (proc c2 -> k -< (c1,c2)) -<< b) -<< b
  Cont f *** Cont g = Cont $ \k -> proc (b1,b2) -> f (proc c1 -> g (proc c2 -> k -< (c1,c2)) -<< b2) -<< b1

instance (ArrowApply c, ArrowChoice c) => ArrowChoice (Cont r c) where
  left (Cont f) = Cont $ \k -> f (k . arr Left) ||| (k . arr Right)
  right (Cont f) = Cont $ \k -> (k . arr Left) ||| f (k . arr Right)
  Cont f ||| Cont g = Cont $ \k -> f k ||| g k
  Cont f +++ Cont g = Cont $ \k -> f (k . arr Left) ||| g (k . arr Right)

instance ArrowApply c => ArrowFix x y (Cont r c) where
  fix f = Cont $ fix (runCont . f . Cont)

instance ArrowLift (Cont r) where
  lift f = Cont $ \k -> k . f

instance (ArrowApply c, ArrowState s c) => ArrowState s (Cont r c) where
  get = lift get
  put = lift put

instance (ArrowApply c, ArrowReader s c) => ArrowReader s (Cont r c) where
  ask = lift ask
  local (Cont f) = Cont $ \k -> local (f k)

instance (ArrowApply c, ArrowWriter w c) => ArrowWriter w (Cont r c) where
  tell = lift tell

instance (ArrowApply c, ArrowFail e c) => ArrowFail e (Cont r c) where
  fail = lift fail

instance (ArrowApply c, ArrowCond v x y r c) => ArrowCond v x y z (Cont r c) where
  if_ (Cont f) (Cont g) = Cont $ \k -> if_ (f k) (g k)
