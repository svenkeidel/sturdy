{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Transformer.Abstract.Pow where

import           Prelude hiding (id,(.))

import           Control.Arrow
import qualified Control.Arrow.Fix as Fix
import           Control.Arrow.Trans
import           Control.Arrow.Environment as Env 
import           Control.Arrow.Store as Store
import           Control.Category
import           Control.Arrow.Monad
import           Control.Comonad

import           Data.Profunctor hiding (map')
import           Data.Abstract.Powerset as Pow 
import           Data.Monoidal

newtype PowT c x y = PowT (c (Pow x) (Pow y))

instance Fix.ArrowFix (Underlying (PowT c) x y) => Fix.ArrowFix (PowT c x y) where
  type Fix (PowT c x y) = Fix.Fix (Underlying (PowT c) x y)

instance ArrowTrans (PowT c) where type Underlying (PowT c) x y = c (Pow x) (Pow y)

instance (ArrowRun c) => ArrowRun (PowT c) where type Run (PowT c) x y = Run c (Pow x) (Pow y)

instance Category c => Category (PowT c) where
  id = lift id
  f . g = lift (unlift f . unlift g)
  {-# INLINE id #-}
  {-# INLINE (.) #-}

instance Profunctor c => Profunctor (PowT c) where
  dimap f g h = lift $ dimap (fmap f) (fmap g) (unlift h)
  lmap f h = lift $ lmap (fmap f) (unlift h)
  rmap g h = lift $ rmap (fmap g) (unlift h)
  {-# INLINE dimap #-}
  {-# INLINE lmap #-}
  {-# INLINE rmap #-}

instance (Arrow c, Profunctor c) => Arrow (PowT c) where
  arr f = lift $ arr (fmap f)
  first f = lift $ dimap Pow.unzip (\(xs,ys) -> crossproduct xs ys) (first $ unlift f)
  second f = lift $ dimap Pow.unzip (\(xs,ys) -> crossproduct xs ys) (second $ unlift f)
  f *** g = lift $ dimap Pow.unzip (\(xs,ys) -> crossproduct xs ys) (unlift f *** unlift g)
  f &&& g = lift $ rmap (\(xs,ys) -> crossproduct xs ys) (unlift f &&& unlift g)
  {-# INLINE arr #-}
  {-# INLINE first #-}
  {-# INLINE second #-}
  {-# INLINE (***) #-}  
  {-# INLINE (&&&) #-}

instance (ArrowComonad Pow c, Profunctor c, ArrowChoice c) => ArrowChoice (PowT c) where
  left f = lift $ dimap costrength1 strength1 (left $ unlift f) -- COMONADS ?? 
  right f = lift $ dimap costrength2 strength2 (right $ unlift f)
  f ||| g = lift $ lmap costrength ((unlift f) ||| (unlift g))
  f +++ g = left f >>> right g
  {-# INLINE left #-}
  {-# INLINE right #-}
  {-# INLINE (|||) #-}  
  {-# INLINE (+++) #-}

instance (ArrowComonad Pow c, ArrowEnv var val c) => ArrowEnv var val (PowT c) where
  type Join y (PowT c) = Env.Join (Pow y) c
  lookup f g = lift $ lmap costrength2 $ Env.lookup (lmap strength2 (unlift f)) (unlift g)
  extend f = lift $ lmap (\m -> let (x,y,_) = extract m in (x,y,fmap (\(_,_,z) -> z) m)) (Env.extend (unlift f))
  {-# INLINE lookup #-}
  {-# INLINE extend #-}

instance (ArrowComonad Pow c, ArrowStore var val c) => ArrowStore var val (PowT c) where
  type Join y (PowT c) = Store.Join (Pow y) c
  read f g = lift $ lmap costrength2 (Store.read (lmap strength2 (unlift f)) (unlift g))
  write = undefined
  {-# INLINE read #-}
  {-# INLINE write #-}

crossproduct :: Pow a -> Pow b -> Pow (a,b)
crossproduct xs ys = do
  x <- xs
  y <- ys
  return (x,y)
{-# INLINE crossproduct #-}