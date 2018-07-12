{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Arrow.Transformer.Cont where

import Prelude hiding (id,(.))

import Control.Category
import Control.Arrow
import Control.Arrow.Fix

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
