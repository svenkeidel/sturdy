{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Arrows #-}
module Control.Arrow.Transformer.Abstract.Pow where

import           Prelude hiding (id,(.))

import           Control.Arrow
import qualified Control.Arrow.Fix as Fix
import           Control.Arrow.Trans
import           Control.Category

import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Profunctor hiding (map')


type Pow a = Seq a
newtype PowT c x y = PowT (c (Pow x) (Pow y))
instance Fix.ArrowFix (Underlying (PowT c) x y) => Fix.ArrowFix (PowT c x y) where
  type Fix (PowT c x y) = Fix.Fix (Underlying (PowT c) x y)
instance ArrowTrans (PowT c) where type Underlying (PowT c) x y = c (Pow x) (Pow y)
instance ArrowRun c => ArrowRun (PowT c) where type Run (PowT c) x y = Run c (Pow x) (Pow y)
instance Category c => Category (PowT c) where
  id = lift id
  f . g = lift (unlift f . unlift g)
instance Profunctor c => Profunctor (PowT c) where
  dimap f g h = lift $ dimap (fmap f) (fmap g) (unlift h)
  lmap f h = lift $ lmap (fmap f) (unlift h)
  rmap g h = lift $ rmap (fmap g) (unlift h)
instance (Arrow c, Profunctor c) => Arrow (PowT c) where
  -- what should happen here ?  
  arr = arr
  first f = lift $ dimap Seq.unzip (\(xs,ys) -> crossproduct xs ys) (first $ unlift f)
  second f = lift $ dimap Seq.unzip (\(xs,ys) -> crossproduct xs ys) (second $ unlift f)
  f *** g = lift $ dimap Seq.unzip (\(xs,ys) -> crossproduct xs ys) (unlift f *** unlift g)
  f &&& g = lift $ rmap (\(xs,ys) -> crossproduct xs ys) (unlift f &&& unlift g)
  {-# INLINE (&&&) #-}
crossproduct :: Pow a -> Pow b -> Pow (a,b)
crossproduct xs ys = do
  x <- xs
  y <- ys
  return (x,y)
{-# INLINE crossproduct #-}
