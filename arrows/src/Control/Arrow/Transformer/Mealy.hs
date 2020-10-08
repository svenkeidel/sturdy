{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Transformer.Mealy where

import Prelude hiding (id,(.))
import Control.Category
import Control.Arrow
import Control.Arrow.Primitive
import Control.Arrow.Trans

import Unsafe.Coerce
import Data.Profunctor hiding (Strong(..))
import Data.Profunctor.Unsafe

-- Due to http://hackage.haskell.org/package/arrows
newtype MealyT c x y = MealyT (c x (y, MealyT c x y))

instance ArrowLift (MealyT c) where
  type Underlying (MealyT c) x y = c x (y, MealyT c x y)

instance (ArrowPrimitive c) => ArrowPrimitive (MealyT c) where
  type PrimState (MealyT c) = PrimState c

instance Profunctor c => Profunctor (MealyT c) where
  lmap f g = lift $ dimap f (second (lmap f)) (unlift g)
  rmap f g = lift $ rmap (f *** rmap f) (unlift g)
  dimap f h g = lift $ dimap f (h *** dimap f h) (unlift g)
  f .# _ = f `seq` unsafeCoerce f
  _ #. g = g `seq` unsafeCoerce g
  {-# INLINABLE lmap #-}
  {-# INLINABLE rmap #-}
  {-# INLINABLE dimap #-}
  {-# INLINE (.#) #-}
  {-# INLINE (#.) #-}

instance ArrowTrans MealyT where
  lift' f = lift (rmap (,lift' f) f)
  {-# INLINABLE lift' #-}

instance (Profunctor c, Arrow c) => Category (MealyT c) where
  id = lift' id
  f . g = lift (rmap (\((z,f'),g') -> (z, f' . g')) (first (unlift f)) . unlift g)
  {-# INLINE id #-}
  {-# INLINABLE (.) #-}

instance (Profunctor c, Arrow c) => Arrow (MealyT c) where
  arr f = lift' $ arr f
  first f = lift $ rmap (\((y,f'),z) -> ((y, z), first f')) $ first (unlift f)
  second f = lift $ rmap (\(y,(z,f')) -> ((y, z), second f')) $ second (unlift f)
  f *** g = lift $ rmap (\((y,f'),(z,g')) -> ((y,z), f' *** g')) $ unlift f *** unlift g
  f &&& g = lift $ rmap (\((y,f'),(z,g')) -> ((y,z), f' &&& g')) $ unlift f &&& unlift g
  {-# INLINE arr #-}
  {-# INLINABLE first #-}
  {-# INLINABLE second #-}
  {-# INLINABLE (&&&) #-}
  {-# INLINABLE (***) #-}

instance (Profunctor c, ArrowChoice c) => ArrowChoice (MealyT c) where
  left f = lift $ rmap (\case { Left (y, f') -> (Left y,left f'); Right z -> (Right z, left f)}) (left (unlift f))
  right f = lift $ rmap (\case { Left y -> (Left y,right f); Right (z,f') -> (Right z, right f')}) (right (unlift f))
  f +++ g = lift $ rmap (\case Left (y, f') -> (Left y, f' +++ g); Right (z, g') -> (Right z, f +++ g')) (unlift f +++ unlift g)
  f ||| g = lift $ rmap (\case Left (y, f') -> (y, f' ||| g); Right (z, g') -> (z, f ||| g')) (unlift f +++ unlift g)
  {-# INLINABLE left #-}
  {-# INLINABLE right #-}
  {-# INLINABLE (+++) #-}
  {-# INLINABLE (|||) #-}

instance (ArrowApply c, Profunctor c) => ArrowApply (MealyT c) where
  app = lift $ dimap (first unlift) (\(c,f') -> (c,lmap (first (const f')) app)) app
  {-# INLINABLE app #-}

