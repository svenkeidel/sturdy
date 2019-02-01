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
-- import Control.Arrow.Conditional
import Data.Profunctor

newtype ContT c x y = ContT { runContT :: forall r. c y r -> c x r }

instance Profunctor c => Profunctor (ContT c) where
  dimap f g (ContT h) = ContT $ \k -> lmap f (h (lmap g k))
  {-# INLINE dimap #-}
  lmap f (ContT h) = ContT $ \k -> lmap f (h k)
  {-# INLINE lmap #-}
  rmap g (ContT h) = ContT $ \k -> h (lmap g k)
  {-# INLINE rmap #-}

instance Category (ContT c) where
  id = ContT id
  {-# INLINE id #-}
  ContT f . ContT g = ContT (g . f)
  {-# INLINE (.) #-}

instance ArrowApply c => Arrow (ContT c) where
  arr f = ContT $ \k -> k . arr f
  {-# INLINE arr #-}
  first (ContT f) = ContT $ \k -> proc (b,d) -> f (proc c -> k -< (c,d)) -<< b
  {-# INLINE first #-}
  second (ContT f) = ContT $ \k -> proc (d,b) -> f (proc c -> k -< (d,c)) -<< b
  {-# INLINE second #-}
  ContT f &&& ContT g = ContT $ \k -> proc b -> f (proc c1 -> g (proc c2 -> k -< (c1,c2)) -<< b) -<< b
  {-# INLINE (&&&) #-}
  ContT f *** ContT g = ContT $ \k -> proc (b1,b2) -> f (proc c1 -> g (proc c2 -> k -< (c1,c2)) -<< b2) -<< b1
  {-# INLINE (***) #-}

instance (ArrowApply c, ArrowChoice c, Profunctor c) => ArrowChoice (ContT c) where
  left (ContT f) = ContT $ \k -> f (lmap Left k) ||| (lmap Right k)
  {-# INLINE left #-}
  right (ContT f) = ContT $ \k -> (lmap Left k) ||| f (lmap Right k)
  {-# INLINE right #-}
  ContT f ||| ContT g = ContT $ \k -> f k ||| g k
  {-# INLINE (|||) #-}
  ContT f +++ ContT g = ContT $ \k -> f (lmap Left k) ||| g (lmap Right k)
  {-# INLINE (+++) #-}

type instance Fix x y (ContT c) = ContT (Fix (Dom ContT x y) (Cod ContT x y) c)
instance (ArrowApply c, ArrowFix x y c) => ArrowFix x y (ContT c) where
  fix = liftFix
  {-# INLINE fix #-}

-- | Lift and unlift proof the yoneda lemma.
instance ArrowTrans ContT where
  type Dom ContT x y = x
  type Cod ContT x y = y
  lift f = ContT $ \k -> k . f
  {-# INLINE lift #-}
  unlift (ContT f) = f id
  {-# INLINE unlift #-}

instance ArrowLift ContT where
  lift' f = ContT $ \k -> k . f
  {-# INLINE lift' #-}

instance (ArrowApply c, ArrowState s c) => ArrowState s (ContT c) where
  get = lift' get
  {-# INLINE get #-}
  put = lift' put
  {-# INLINE put #-}

instance (ArrowApply c, ArrowReader s c) => ArrowReader s (ContT c) where
  ask = lift' ask
  {-# INLINE ask #-}
  local (ContT f) = ContT $ \k -> local (f k)
  {-# INLINE local #-}

instance (ArrowApply c, ArrowWriter w c) => ArrowWriter w (ContT c) where
  tell = lift' tell
  {-# INLINE tell #-}

instance (ArrowApply c, ArrowFail e c) => ArrowFail e (ContT c) where
  fail = lift' fail
  {-# INLINE fail #-}

-- instance (ArrowApply c, ArrowCond v c) => ArrowCond v (ContT c) where
--   type Join (ContT c) x y = Cond.Join c (Dom1 ContT x y) (Cod1 ContT x y)
--   if_ (ContT f) (ContT g) = ContT $ \k -> if_ (f k) (g k)
