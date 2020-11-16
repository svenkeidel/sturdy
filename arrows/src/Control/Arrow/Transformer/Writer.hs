{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Arrow.Transformer.Writer where

import Prelude hiding (id,(.),lookup,read,fail)

import Control.Category
import Control.Arrow
import Control.Arrow.Const
import Control.Arrow.Primitive
import Control.Arrow.Reader as Reader
import Control.Arrow.State as State
import Control.Arrow.Trans
import Control.Arrow.Writer

import Data.Monoidal
import Data.Profunctor
import Data.Profunctor.Unsafe
import Data.Coerce
import Unsafe.Coerce

-- | Arrow transformer that adds a writable value to an arrow computation.
newtype WriterT w c x y = WriterT { runWriterT :: c x (w,y) }

censor :: (Arrow c,Profunctor c) => (x -> w -> w) -> WriterT w c x y -> WriterT w c x y
censor f (WriterT g) = WriterT (dimap (\x -> (x,x)) (\(x,(w,y)) -> (f x w,y)) (second g))

instance (Monoid w,ArrowRun c) => ArrowRun (WriterT w c) where
  type Run (WriterT w c) x y = Run c x (w,y)

instance ArrowLift (WriterT w c) where
  type Underlying (WriterT w c) x y = c x (w,y)

instance (Monoid w,ArrowPrimitive c) => ArrowPrimitive (WriterT w c) where
  type PrimState (WriterT w c) = PrimState c

instance (Profunctor c) => Profunctor (WriterT w c) where
  dimap f g h = lift $ dimap f (second g) (unlift h)
  lmap f h = lift $ lmap f (unlift h)
  rmap g h = lift $ rmap (second g) (unlift h)
  f .# _ = f `seq` unsafeCoerce f
  _ #. g = g `seq` unsafeCoerce g
  {-# INLINE dimap #-}
  {-# INLINE lmap #-}
  {-# INLINE rmap #-}
  {-# INLINE (.#) #-}
  {-# INLINE (#.) #-}

instance Monoid w => ArrowTrans (WriterT w) where
  lift' f = lift (rmap (mempty,) f)
  {-# INLINE lift' #-}

instance (Monoid w, Arrow c, Profunctor c) => Category (WriterT w c) where
  id = lift' id
  g . f = lift $ rmap (\(w1,(w2,z)) -> (w1 <> w2,z)) (unlift f >>> second (unlift g)) 
  -- proc x -> do
  --   (w1,y) <- f -< x
  --   (w2,z) <- g -< y
  --   returnA -< (w1 <> w2,z)
  {-# INLINE id #-}
  {-# INLINE (.) #-}

instance (Monoid w, Arrow c, Profunctor c) => Arrow (WriterT w c) where
  arr f = lift' (arr f)
  first f = lift $ rmap assoc2 (first (unlift f)) 
  second g = lift $ rmap shuffle2 (second (unlift g)) 
  f *** g = lift $ rmap (\((w1,b),(w2,d)) -> (w1 <> w2,(b,d))) (unlift f *** unlift g) 
  f &&& g = lift $ rmap (\((w1,b),(w2,d)) -> (w1 <> w2,(b,d))) (unlift f &&& unlift g)
  {-# INLINE arr #-}
  {-# INLINE first #-}
  {-# INLINE second #-}
  {-# INLINE (&&&) #-}
  {-# INLINE (***) #-}

instance (Monoid w, ArrowChoice c, Profunctor c) => ArrowChoice (WriterT w c) where
  left f = lift $ rmap (\e -> case e of Left (w,x) -> (w,Left x); Right y -> (mempty,Right y)) (left (unlift f)) 
  right f = lift $ rmap (\e -> case e of Left x -> (mempty,Left x); Right (w,y) -> (w,Right y)) (right (unlift f))
  f ||| g = lift $ unlift f ||| unlift g
  f +++ g = lift $ rmap distribute2 (unlift f +++ unlift g) 
  {-# INLINE left #-}
  {-# INLINE right #-}
  {-# INLINE (+++) #-}
  {-# INLINE (|||) #-}

instance (Monoid w, ArrowApply c, Profunctor c) => ArrowApply (WriterT w c) where
  app = lift (app .# first coerce)
  {-# INLINE app #-}

instance (Monoid w, ArrowState s c) => ArrowState s (WriterT w c) where
  get = lift' get
  put = lift' put
  modify f = lift $ modify (rmap assoc1 (unlift f))
  {-# INLINE get #-}
  {-# INLINE put #-}
  {-# INLINE modify #-}

instance (Monoid w, Arrow c, Profunctor c) => ArrowWriter w (WriterT w c) where
  tell = lift (arr (\w -> (w,())))
  {-# INLINE tell #-}

instance (Monoid w, ArrowReader r c) => ArrowReader r (WriterT w c) where
  ask = lift' Reader.ask
  local f = lift (Reader.local (unlift f))
  {-# INLINE ask #-}
  {-# INLINE local #-}

instance (Monoid w, ArrowConst x c) => ArrowConst x (WriterT w c) where
  askConst f = lift (askConst (unlift . f))
  {-# INLINE askConst #-}

