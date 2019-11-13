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
import Control.Arrow.Cont
import Control.Arrow.Fix
import Control.Arrow.Fail
import Control.Arrow.Order
import Control.Arrow.Trans
import Control.Arrow.Reader
import Control.Arrow.State
import Control.Arrow.Writer

import Data.Profunctor
import Data.Profunctor.Unsafe

import Unsafe.Coerce

newtype ContT r c x y = ContT { runContT :: c y r -> c x r }

instance (ArrowApply c, Profunctor c) => ArrowCont (ContT r c) where
  type Cont (ContT r c) x = c x r
  callCC f = lift $ \k -> unlift (f k) k
  jump k = lift $ \_ -> k
  {-# INLINE callCC #-}
  {-# INLINE jump #-}

instance (ArrowApply c, ArrowRun c) => ArrowRun (ContT r c) where
  type Run (ContT r c) x y = c y r -> Run c x r
  run f x = run $ runContT f x
  {-# INLINE run #-}

instance ArrowTrans (ContT r c) where
  type Underlying (ContT r c) x y = c y r -> c x r

instance ArrowLift (ContT r) where
  lift' f = ContT $ \k -> k . f
  {-# INLINE lift' #-}

instance Profunctor c => Profunctor (ContT r c) where
  dimap f g h = lift $ \k -> lmap f (unlift h (lmap g k))
  lmap f h = lift $ \k -> lmap f (unlift h k)
  rmap g h = lift $ \k -> unlift h (lmap g k)
  f .# _ = f `seq` unsafeCoerce f
  _ #. g = g `seq` unsafeCoerce g
  {-# INLINE dimap #-}
  {-# INLINE lmap #-}
  {-# INLINE rmap #-}
  {-# INLINE (.#) #-}
  {-# INLINE (#.) #-}

instance Category (ContT r c) where
  id = lift id
  f . g = lift (unlift g . unlift f)
  {-# INLINE id #-}
  {-# INLINE (.) #-}

instance ArrowApply c => Arrow (ContT r c) where
  arr f = lift $ \k -> k . arr f
  first f = lift $ \k -> proc (b,d) -> unlift f (proc c -> k -< (c,d)) -<< b
  second f = lift $ \k -> proc (d,b) -> unlift f (proc c -> k -< (d,c)) -<< b
  f &&& g = lift $ \k -> proc b -> unlift f (proc c1 -> unlift g (proc c2 -> k -< (c1,c2)) -<< b) -<< b
  f *** g = lift $ \k -> proc (b1,b2) -> unlift f (proc c1 -> unlift g (proc c2 -> k -< (c1,c2)) -<< b2) -<< b1
  {-# INLINE arr #-}
  {-# INLINE first #-}
  {-# INLINE second #-}
  {-# INLINE (&&&) #-}
  {-# INLINE (***) #-}

instance (ArrowApply c, ArrowChoice c, Profunctor c) => ArrowChoice (ContT r c) where
  left f = lift $ \k -> unlift f (lmap Left k) ||| lmap Right k
  right f = lift $ \k -> lmap Left k ||| unlift f (lmap Right k)
  f ||| g = lift $ \k -> unlift f k ||| unlift g k
  f +++ g = lift $ \k -> unlift f (lmap Left k) ||| unlift g (lmap Right k)
  {-# INLINE left #-}
  {-# INLINE right #-}
  {-# INLINE (|||) #-}
  {-# INLINE (+++) #-}

instance ArrowApply c => ArrowApply (ContT r c) where
  app = lift $ \k -> proc (f,x) -> app -< (unlift f k, x)
  {-# INLINE app #-}

type instance Fix (ContT _ c) x y = ContT y (Fix c x y)
instance ArrowFix (c x r) => ArrowFix (ContT r c x y) where
  fix f = lift $ \k -> fix $ \g -> unlift1 f (const g) k
  {-# INLINE fix #-}

-- instance (ArrowApply c, ArrowJoin c, ArrowComplete r c) => ArrowJoin (ContT r c) where
--   joinSecond _ _ x = lift $ f <⊔> arr g

instance (ArrowApply c, ArrowComplete r c) => ArrowComplete y (ContT r c) where
  (<⊔>) f g = lift $ \k -> unlift f k <⊔> unlift g k

instance (ArrowApply c, ArrowState s c) => ArrowState s (ContT r c) where
  get = lift' get
  put = lift' put
  {-# INLINE get #-}
  {-# INLINE put #-}

instance (ArrowApply c, ArrowReader s c) => ArrowReader s (ContT r c) where
  ask = lift' ask
  local f = lift $ \k -> local (unlift f k)
  {-# INLINE ask #-}
  {-# INLINE local #-}

instance (ArrowApply c, ArrowWriter w c) => ArrowWriter w (ContT r c) where
  tell = lift' tell
  {-# INLINE tell #-}

instance (ArrowApply c, ArrowFail e c) => ArrowFail e (ContT r c) where
  fail = lift' fail
  {-# INLINE fail #-}
