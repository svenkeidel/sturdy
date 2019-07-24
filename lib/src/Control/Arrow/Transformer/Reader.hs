{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Control.Arrow.Transformer.Reader(ReaderT(..)) where

import Prelude hiding (id,(.),lookup,read,fail)

import Control.Arrow
import Control.Arrow.Const
import Control.Arrow.Environment as Env
import Control.Arrow.Fail
import Control.Arrow.Fix
import Control.Arrow.Reader as Reader
import Control.Arrow.Store as Store
import Control.Arrow.State as State
import Control.Arrow.Except as Exc
import Control.Arrow.Trans
import Control.Arrow.Writer
import Control.Arrow.Order
import Control.Category

import Data.Profunctor
import Data.Profunctor.Unsafe
import Data.Monoidal
import Data.Coerce
import Unsafe.Coerce

-- Due to "Generalising Monads to Arrows", by John Hughes, in Science of Computer Programming 37.
newtype ReaderT r c x y = ReaderT { runReaderT :: c (r,x) y }

instance ArrowRun c => ArrowRun (ReaderT r c) where
  type Rep (ReaderT r c) x y = Rep c (r,x) y
  run = run . runReaderT

instance (Profunctor c, Arrow c) => Profunctor (ReaderT r c) where
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

instance ArrowTrans (ReaderT r) where
  type Dom (ReaderT r) x y = (r,x)
  type Cod (ReaderT r) x y = y
  lift = coerce
  unlift = coerce
  {-# INLINE lift #-}
  {-# INLINE unlift #-}

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
  type instance Join y (ReaderT r c) = Env.Join y c
  lookup f g = lift $ lmap shuffle1
                    $ lookup (lmap shuffle1 (unlift f)) (unlift g)
  extend f = lift $ lmap (\(r,(var,val,x)) -> (var,val,(r,x))) (Env.extend (unlift f))
  {-# INLINE lookup #-}
  {-# INLINE extend #-}

instance ArrowClosure var val env c => ArrowClosure var val env (ReaderT r c) where
  ask     = lift' Env.ask
  local f = lift $ lmap shuffle1 $ Env.local (unlift f)
  {-# INLINE ask #-}
  {-# INLINE local #-}

instance ArrowStore var val c => ArrowStore var val (ReaderT r c) where
  type instance Join y (ReaderT r c) = Store.Join y c
  read f g = lift $ lmap shuffle1 
                  $ read (lmap shuffle1 (unlift f)) (unlift g)
  write = lift' write
  {-# INLINE read #-}
  {-# INLINE write #-}

type instance Fix x y (ReaderT r c) = ReaderT r (Fix (Dom (ReaderT r) x y) (Cod (ReaderT r) x y) c)
instance ArrowFix (Dom (ReaderT r) x y) (Cod (ReaderT r) x y) c => ArrowFix x y (ReaderT r c) where
  fix = liftFix
  {-# INLINE fix #-}

instance ArrowExcept e c => ArrowExcept e (ReaderT r c) where
  type instance Join z (ReaderT r c) = Exc.Join z c
  throw = lift' throw
  try f g h = lift $ try (lmap (\(r,x) -> (r,(r,x))) (second (unlift f))) (unlift g) (lmap assoc2 (unlift h))
  {-# INLINE throw #-}
  {-# INLINE try #-}

instance ArrowLowerBounded c => ArrowLowerBounded (ReaderT r c) where
  bottom = ReaderT bottom
  {-# INLINE bottom #-}

instance ArrowJoin c => ArrowJoin (ReaderT r c) where
  join lub f g = lift $ join lub (unlift f) (unlift g)
  {-# INLINE join #-}

instance ArrowComplete y c => ArrowComplete y (ReaderT r c) where
  f <⊔> g = lift $ unlift f <⊔> unlift g
  {-# INLINE (<⊔>) #-}

instance ArrowConst x c => ArrowConst x (ReaderT r c) where
  askConst f = lift (askConst (unlift . f))
  {-# INLINE askConst #-}
