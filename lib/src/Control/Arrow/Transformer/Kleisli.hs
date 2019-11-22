{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Transformer.Kleisli where

import Prelude hiding (id,(.),lookup,read,fail)

import Control.Category
import Control.Arrow hiding (ArrowMonad)
import Control.Arrow.Cont
import Control.Arrow.Const
import Control.Arrow.Environment as Env
import Control.Arrow.Closure as Cls
import Control.Arrow.Except as Exc
import Control.Arrow.Fail
import Control.Arrow.Fix
import Control.Arrow.Monad
import Control.Arrow.Order as Ord
import Control.Arrow.Reader as Reader
import Control.Arrow.State as State
import Control.Arrow.Store as Store
import Control.Arrow.Trans

import Data.Monoidal
import Data.Profunctor (Profunctor(..))
import Data.Profunctor.Unsafe
import Data.Coerce
import Unsafe.Coerce

newtype KleisliT f c x y = KleisliT { runKleisliT :: c x (f y) }

instance (ArrowMonad f c, ArrowRun c) => ArrowRun (KleisliT f c) where type Run (KleisliT f c) x y = Run c x (f y)
instance ArrowTrans (KleisliT f c) where type Underlying (KleisliT f c) x y = c x (f y)

instance Monad f => ArrowLift (KleisliT f) where
  lift' f = lift $ rmap return f
  {-# INLINE lift' #-}

instance ArrowMonad f c => Profunctor (KleisliT f c) where
  dimap f g h = lift $ dimap f (fmap g) $ unlift h
  lmap f h = lift $ lmap f (unlift h)
  rmap g h = lift $ rmap (fmap g) (unlift h)
  f .# _ = unsafeCoerce f
  _ #. g = unsafeCoerce g
  {-# INLINE dimap #-}
  {-# INLINE lmap #-}
  {-# INLINE rmap #-}
  {-# INLINE (.#) #-}
  {-# INLINE (#.) #-}

instance ArrowMonad f c => Category (KleisliT f c) where
  id = lift unitA
  f . g = lift $ mapJoinA (unlift f) . unlift g
  {-# INLINE id #-}
  {-# INLINE (.) #-}

instance ArrowMonad f c => Arrow (KleisliT f c) where
  arr f = lift $ arr (return . f)
  first f = lift $ rmap strength1 (first (unlift f))
  second f = lift $ rmap strength2 (second (unlift f))
  f &&& g = lmap (\x -> (x,x)) (f *** g)
  f *** g = first f >>> second g
  {-# INLINE arr #-}
  {-# INLINE first #-}
  {-# INLINE second #-}
  {-# INLINE (&&&) #-}
  {-# INLINE (***) #-}

instance (ArrowMonad f c, ArrowChoice c) => ArrowChoice (KleisliT f c) where
  left f = lift $ rmap strength1 $ left (unlift f)
  right f = lift $ rmap strength2 $ right (unlift f)
  f ||| g = lift $ unlift f ||| unlift g
  f +++ g = left f >>> right g
  {-# INLINE left #-}
  {-# INLINE right #-}
  {-# INLINE (+++) #-}
  {-# INLINE (|||) #-}

instance (ArrowMonad f c, ArrowApply c) => ArrowApply (KleisliT f c) where
  app = lift (app .# first coerce)
  {-# INLINE app #-}

instance (ArrowCont c, Monad f, ArrowMonad f c) => ArrowCont (KleisliT f c) where
  type Cont (KleisliT f c) y = Cont c (f y)
  callCC f = lift $ callCC $ \k -> unlift (f k)
  jump k = lift $ lmap (return @f) (jump k)
  {-# INLINE callCC #-}

instance (ArrowMonad f c, ArrowState s c) => ArrowState s (KleisliT f c) where
  get = lift' State.get
  put = lift' State.put
  {-# INLINE get #-}
  {-# INLINE put #-}

instance (ArrowMonad f c, ArrowReader r c) => ArrowReader r (KleisliT f c) where
  ask = lift' Reader.ask
  local f = lift (Reader.local (unlift f))
  {-# INLINE ask #-}
  {-# INLINE local #-}

instance (ArrowMonad f c, ArrowEnv x y c) => ArrowEnv x y (KleisliT f c) where
  type Join y (KleisliT f c) = Env.Join (f y) c
  lookup f g = lift $ Env.lookup (unlift f) (unlift g)
  extend f = lift $ Env.extend (unlift f)
  {-# INLINE lookup #-}
  {-# INLINE extend #-}

instance (ArrowMonad f c, ArrowLetRec x y c) => ArrowLetRec x y (KleisliT f c) where
  letRec f = lift (letRec (unlift f))
  {-# INLINE letRec #-}

instance (ArrowMonad f c, ArrowClosure expr cls c) => ArrowClosure expr cls (KleisliT f c) where
  type Join y (KleisliT f c) = Cls.Join (f y) c
  apply f = lift (Cls.apply (unlift f))
  {-# INLINE apply #-}

instance (ArrowMonad f c, ArrowStore var val c) => ArrowStore var val (KleisliT f c) where
  type Join y (KleisliT f c) = Store.Join (f y) c
  read f g = lift $ read (unlift f) (unlift g)
  write = lift' write
  {-# INLINE read #-}
  {-# INLINE write #-}

type instance Fix (KleisliT f c) x y = KleisliT f (Fix c y (f y))
instance ArrowFix (c x (f y)) => ArrowFix (KleisliT f c x y) where

instance (ArrowMonad f c, ArrowExcept e c) => ArrowExcept e (KleisliT f c) where
  type Join y (KleisliT f c) = Exc.Join (f y) c
  throw = lift' throw
  try f g h = lift $ try (unlift f) (mapJoinA (unlift g)) (unlift h)
  {-# INLINE throw #-}
  {-# INLINE try #-}

instance (ArrowMonad f c, ArrowFail e c) => ArrowFail e (KleisliT f c) where
  fail = lift' fail
  {-# INLINE fail #-}

instance (ArrowMonad f c, ArrowConst r c) => ArrowConst r (KleisliT f c) where
  askConst f = lift (askConst (unlift . f))
  {-# INLINE askConst #-}

instance (ArrowMonad f c, ArrowComplete (f y) c) => ArrowComplete y (KleisliT f c) where
  f <⊔> g = lift $ unlift f <⊔> unlift g
  {-# INLINE (<⊔>) #-}

instance (ArrowMonad f c, ArrowLowerBounded c) => ArrowLowerBounded (KleisliT f c) where
  bottom = lift Ord.bottom
