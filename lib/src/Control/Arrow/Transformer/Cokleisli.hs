{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Transformer.Cokleisli where

import Prelude hiding (id,(.),lookup,read,fail)

import Control.Category
import Control.Arrow hiding (ArrowMonad)
import Control.Arrow.Const
import Control.Arrow.Environment as Env
import Control.Arrow.Closure as Cls
import Control.Arrow.Except as Exc
import Control.Arrow.Fail
import Control.Arrow.Fix
import Control.Arrow.Monad
import Control.Arrow.Order
import Control.Arrow.Reader as Reader
import Control.Arrow.State as State
import Control.Arrow.Store as Store
import Control.Arrow.Trans

import Data.Monoidal
import Data.Profunctor (Profunctor(..))
import Data.Profunctor.Unsafe
import Unsafe.Coerce
import Control.Comonad

newtype CokleisliT f c x y = CokleisliT { runCokleisliT :: c (f x) y }

instance (ArrowComonad f c, ArrowRun c) => ArrowRun (CokleisliT f c) where
  type Run (CokleisliT f c) x y = Run c (f x) y
instance ArrowTrans (CokleisliT f c) where
  type Underlying (CokleisliT f c) x y = c (f x) y

instance Comonad f => ArrowLift (CokleisliT f) where
  lift' f = lift $ lmap extract f
  {-# INLINE lift' #-}

instance (ArrowComonad f c) => Profunctor (CokleisliT f c) where
  dimap f g h = lift $ dimap (fmap f) g $ unlift h
  lmap f h = lift $ lmap (fmap f) (unlift h)
  rmap g h = lift $ rmap g (unlift h)
  f .# _ = unsafeCoerce f
  _ #. g = unsafeCoerce g
  {-# INLINE dimap #-}
  {-# INLINE lmap #-}
  {-# INLINE rmap #-}
  {-# INLINE (.#) #-}
  {-# INLINE (#.) #-}

instance ArrowComonad f c => Category (CokleisliT f c) where
  id = lift extractA
  f . g = lift $ unlift f . mapDuplicateA (unlift g)
  {-# INLINE id #-}
  {-# INLINE (.) #-}

instance (ArrowComonad f c) => Arrow (CokleisliT f c) where
  arr f = lift $ rmap f extractA
  first f = lift $ lmap costrength1 (first (unlift f))
  second f = lift $ lmap costrength2 (second (unlift f))
  f &&& g = lmap (\x -> (x,x)) (f *** g)
  f *** g = first f >>> second g
  {-# INLINE arr #-}
  {-# INLINE first #-}
  {-# INLINE second #-}
  {-# INLINE (&&&) #-}
  {-# INLINE (***) #-}

instance (ArrowComonad f c, ArrowChoice c) => ArrowChoice (CokleisliT f c) where
  left f = lift $ lmap costrength1 (left (unlift f))
  right f = lift $ lmap costrength2 (right (unlift f))
  f ||| g = lift $ lmap costrength (unlift f ||| unlift g)
  f +++ g = lift $ lmap costrength (unlift f +++ unlift g)
  {-# INLINE left #-}
  {-# INLINE right #-}
  {-# INLINE (+++) #-}
  {-# INLINE (|||) #-}

instance (ArrowComonad f c, ArrowState s c) => ArrowState s (CokleisliT f c) where
  get = lift' State.get
  put = lift' State.put
  {-# INLINE get #-}
  {-# INLINE put #-}

instance (ArrowComonad f c, ArrowReader r c) => ArrowReader r (CokleisliT f c) where
  ask = lift' Reader.ask
  local f = lift (lmap costrength2 (Reader.local (unlift f)))
  {-# INLINE ask #-}
  {-# INLINE local #-}

instance (ArrowComonad f c, ArrowEnv x y c) => ArrowEnv x y (CokleisliT f c) where
  type Join y (CokleisliT f c) = Env.Join y c
  lookup f g = lift $ lmap costrength2 (Env.lookup (lmap strength2 (unlift f)) (unlift g))
  extend f = lift $ lmap (\m -> let (x,y,_) = extract m in (x,y,fmap (\(_,_,z) -> z) m)) (Env.extend (unlift f))
  {-# INLINE lookup #-}
  {-# INLINE extend #-}

instance (ArrowComonad f c, ArrowClosure expr cls c) => ArrowClosure expr cls (CokleisliT f c) where
  type Join y (CokleisliT f c) = Cls.Join y c
  apply f = lift (lmap costrength2 (Cls.apply (lmap strength2 (unlift f))))
  {-# INLINE apply #-}

instance (ArrowComonad f c, ArrowStore var val c) => ArrowStore var val (CokleisliT f c) where
  type Join y (CokleisliT f c) = Store.Join y c
  read f g = lift $ lmap costrength2 (read (lmap strength2 (unlift f)) (unlift g))
  write = lift' write
  {-# INLINE read #-}
  {-# INLINE write #-}

type instance Fix (CokleisliT f c) x y = CokleisliT f (Fix c (f x) y)
instance (ArrowComonad f c, ArrowFix (Underlying (CokleisliT f c) x y)) => ArrowFix (CokleisliT f c x y)

instance (ArrowComonad f c, ArrowExcept e c) => ArrowExcept e (CokleisliT f c) where
  type Join y (CokleisliT f c) = Exc.Join y c
  throw = lift' throw
  try f g h = lift $ try (mapDuplicateA (unlift f)) (unlift g) (lmap strength1 (unlift h))
  {-# INLINE throw #-}
  {-# INLINE try #-}

instance (ArrowComonad f c, ArrowFail e c) => ArrowFail e (CokleisliT f c) where
  fail = lift' fail
  {-# INLINE fail #-}

instance (ArrowComonad f c, ArrowConst r c) => ArrowConst r (CokleisliT f c) where
  askConst f = lift (askConst (unlift . f))
  {-# INLINE askConst #-}

instance (ArrowComonad f c, ArrowComplete y c) => ArrowComplete y (CokleisliT f c) where
  f <⊔> g = lift $ unlift f <⊔> unlift g
  {-# INLINE (<⊔>) #-}

instance (ArrowComonad f c, ArrowJoin c) => ArrowJoin (CokleisliT f c) where
  joinSecond lub f g = lift $ joinSecond lub (f . extract) (unlift g)
  {-# INLINE joinSecond #-}

