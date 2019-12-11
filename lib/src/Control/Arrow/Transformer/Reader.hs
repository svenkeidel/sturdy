{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Control.Arrow.Transformer.Reader(ReaderT(..)) where

import Prelude hiding (id,(.),lookup,read,fail)

import Control.Category
import Control.Arrow
import Control.Arrow.Cont
import Control.Arrow.Const
import Control.Arrow.Environment as Env
import Control.Arrow.Closure as Cls
import Control.Arrow.Except as Exc
import Control.Arrow.Fail
import Control.Arrow.Fix
import Control.Arrow.Fix.Parallel as Parallel
import Control.Arrow.Fix.Cache as Cache
import Control.Arrow.Fix.Context as Context
import Control.Arrow.Order
import Control.Arrow.Reader as Reader
import Control.Arrow.State as State
import Control.Arrow.Store as Store
import Control.Arrow.Trans
import Control.Arrow.Writer

import Data.Profunctor
import Data.Profunctor.Unsafe
import Data.Monoidal
import Unsafe.Coerce

-- Due to "Generalising Monads to Arrows", by John Hughes, in Science of Computer Programming 37.
newtype ReaderT r c x y = ReaderT { runReaderT :: c (r,x) y }

instance ArrowRun c => ArrowRun (ReaderT r c) where type Run (ReaderT r c) x y = Run c (r,x) y
instance ArrowTrans (ReaderT r c) where type Underlying (ReaderT r c) x y = c (r,x) y

instance (Profunctor c) => Profunctor (ReaderT r c) where
  dimap f g h = lift $ dimap (second f) g (unlift h)
  lmap f h = lift $ lmap (second f) (unlift h)
  rmap g h = lift $ rmap g (unlift h)
  f .# _ = f `seq` unsafeCoerce f
  _ #. g = g `seq` unsafeCoerce g
  {-# INLINE dimap #-}
  {-# INLINE lmap #-}
  {-# INLINE rmap #-}
  {-# INLINE (.#) #-}
  {-# INLINE (#.) #-}

instance ArrowLift (ReaderT r) where
  lift' f = lift $ lmap snd f
  {-# INLINE lift' #-}

instance (Arrow c, Profunctor c) => Category (ReaderT r c) where
  id    = lift' id
  f . g = lift $ lmap (\(r,x) -> (r,(r,x))) (unlift f . second (unlift g))
  {-# INLINE id #-}
  {-# INLINE (.) #-}

instance (Arrow c, Profunctor c) => Arrow (ReaderT r c) where
  arr f    = lift' (arr f)
  first f  = lift $ lmap assoc1 $ first (unlift f)
  second f = lift $ lmap shuffle1 $ second (unlift f)
  f &&& g  = lift $ unlift f &&& unlift g
  f *** g  = lift $ lmap (\(r,(b,d)) -> ((r,b),(r,d))) $ unlift f *** unlift g
  {-# INLINE arr #-}
  {-# INLINE first #-}
  {-# INLINE second #-}
  {-# INLINE (&&&) #-}
  {-# INLINE (***) #-}

instance (ArrowChoice c, Profunctor c) => ArrowChoice (ReaderT r c) where
  left f  = lift $ lmap (\(r,e) -> left (r,) e) $ left (unlift f)
  right f = lift $ lmap (\(r,e) -> right (r,) e) $ right (unlift f)
  f +++ g = lift $ lmap distribute1 $ unlift f +++ unlift g
  f ||| g = lift $ lmap distribute1 $ unlift f ||| unlift g
  {-# INLINE left #-}
  {-# INLINE right #-}
  {-# INLINE (+++) #-}
  {-# INLINE (|||) #-}

instance (ArrowApply c, Profunctor c) => ArrowApply (ReaderT r c) where
  app = lift $ lmap (\(r,(f,b)) -> (unlift f,(r,b))) app
  {-# INLINE app #-}

instance (ArrowCont c, Profunctor c) => ArrowCont (ReaderT r c) where
  type Cont (ReaderT r c) y = Cont c y
  callCC f = lift $ callCC $ \k -> unlift (f k)
  jump k = lift $ lmap snd $ jump k
  {-# INLINE callCC #-}

instance (Arrow c, Profunctor c) => ArrowReader r (ReaderT r c) where
  ask = lift (arr fst)
  local f = lift $ lmap snd (unlift f)
  {-# INLINE ask #-}
  {-# INLINE local #-}

instance ArrowState s c => ArrowState s (ReaderT r c) where
  get = lift' State.get
  put = lift' State.put
  modify f = lift (modify (lmap assoc2 (unlift f)))
  {-# INLINE get #-}
  {-# INLINE put #-}
  {-# INLINE modify #-}

instance ArrowWriter w c => ArrowWriter w (ReaderT r c) where
  tell = lift' tell
  {-# INLINE tell #-}

instance ArrowFail e c => ArrowFail e (ReaderT r c) where
  fail = lift' fail
  {-# INLINE fail #-}

instance ArrowEnv var val c => ArrowEnv var val (ReaderT r c) where
  type Join y (ReaderT r c) = Env.Join y c
  lookup f g = lift $ lmap shuffle1
                    $ Env.lookup (lmap shuffle1 (unlift f)) (unlift g)
  extend f = lift $ lmap (\(r,(var,val,x)) -> (var,val,(r,x))) (Env.extend (unlift f))
  {-# INLINE lookup #-}
  {-# INLINE extend #-}

instance ArrowClosure expr cls c => ArrowClosure expr cls (ReaderT r c) where
  type Join y (ReaderT r c) = Cls.Join y c
  apply f = lift $ lmap shuffle1 $ Cls.apply (lmap shuffle1 (unlift f))
  {-# INLINE apply #-}

instance ArrowStore var val c => ArrowStore var val (ReaderT r c) where
  type Join y (ReaderT r c) = Store.Join y c
  read f g = lift $ lmap shuffle1
                  $ Store.read (lmap shuffle1 (unlift f)) (unlift g)
  write = lift' Store.write
  {-# INLINE read #-}
  {-# INLINE write #-}

type instance Fix (ReaderT r c) x y = ReaderT r (Fix c (r,x) y)
instance ArrowFix (Underlying (ReaderT r c) x y) => ArrowFix (ReaderT r c x y)

instance ArrowExcept e c => ArrowExcept e (ReaderT r c) where
  type Join z (ReaderT r c) = Exc.Join z c
  throw = lift' throw
  try f g h = lift $ try (lmap (\(r,x) -> (r,(r,x))) (second (unlift f))) (unlift g) (lmap assoc2 (unlift h))
  {-# INLINE throw #-}
  {-# INLINE try #-}

instance ArrowLowerBounded c => ArrowLowerBounded (ReaderT r c) where
  bottom = ReaderT bottom
  {-# INLINE bottom #-}

instance ArrowJoin c => ArrowJoin (ReaderT r c) where
  joinSecond lub f g = lift $ joinSecond lub (f . snd) (unlift g)
  {-# INLINE joinSecond #-}

instance ArrowComplete y c => ArrowComplete y (ReaderT r c) where
  f <⊔> g = lift $ unlift f <⊔> unlift g
  {-# INLINE (<⊔>) #-}

instance ArrowConst x c => ArrowConst x (ReaderT r c) where
  askConst f = lift (askConst (unlift . f))
  {-# INLINE askConst #-}

instance ArrowEffectCommutative c => ArrowEffectCommutative (ReaderT r c)

instance ArrowContext ctx c => ArrowContext ctx (ReaderT r c) where
  localContext f = lift $ lmap shuffle1 (localContext (unlift f))
  {-# INLINE localContext #-}

instance ArrowJoinContext a c => ArrowJoinContext a (ReaderT r c)
instance (ArrowCache a b c) => ArrowCache a b (ReaderT r c)
instance (ArrowParallel c) => ArrowParallel (ReaderT r c)
