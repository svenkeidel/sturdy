{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Transformer.ST(runST, ST(..)) where

import           Prelude hiding (id,(.),lookup,read,fail)

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Trans
import           Control.Arrow.Primitive

import           Unsafe.Coerce
import           Data.Profunctor hiding (Strong(..))
import           Data.Profunctor.Unsafe

import           GHC.Exts

-- Arrow version of the ST monad (https://hackage.haskell.org/package/base/docs/Control-Monad-ST.html).
newtype ST s x y = ST ( (# State# s, x #) -> (# State# s, y #) )

instance ArrowPrimitive (ST s) where
  type PrimState (ST s) = s
  primitive = lift
  {-# INLINE primitive #-}

runST :: (forall s. ST s x y) -> (x -> y)
runST (ST f) x = case runRW# (\s -> f (# s, x #)) of { (# _, y #) -> y }

instance ArrowRun (ST s) where
  type Run (ST s) x y = ST s x y
  run f = f
  {-# NOINLINE run #-}

instance ArrowLift (ST s) where
  type Underlying (ST s) x y = (# State# s, x #) -> (# State# s, y #)

instance Profunctor (ST s) where
  dimap f g h = arr g . h . arr f
  lmap f h = h . arr f
  rmap g h = arr g . h
  f .# _ = f `seq` unsafeCoerce f
  _ #. g = g `seq` unsafeCoerce g
  {-# INLINE dimap #-}
  {-# INLINE lmap #-}
  {-# INLINE rmap #-}
  {-# INLINE ( .# ) #-}
  {-# INLINE ( #. ) #-}

instance Category (ST c) where
  id = lift $ \x -> x
  f . g = lift $ \x -> unlift f (unlift g x)
  {-# INLINE id #-}
  {-# INLINE (.) #-}

instance Arrow (ST s) where
  arr f = lift $ \(# s, x #) -> (# s, f x #)
  first f = lift $ \(# s, (x,z) #) -> case unlift f (# s, x #) of { (# s', y #) -> (# s', (y,z) #)}
  second f = lift $ \(# s, (z,x) #) -> case unlift f (# s, x #) of { (# s', y #) -> (# s', (z,y) #)}
  f &&& g = lift $ \(# s, x #) -> case unlift f (# s, x #) of { (# s', y #) -> case unlift g (# s', x #) of { (# s'', z #) -> (# s'', (y,z) #)}}
  f *** g = lift $ \(# s, (x,x') #) -> case unlift f (# s, x #) of { (# s', y #) -> case unlift g (# s', x' #) of { (# s'', z #) -> (# s'', (y,z) #)}}
  {-# INLINE arr #-}
  {-# INLINE first #-}
  {-# INLINE second #-}
  {-# INLINE (&&&) #-}
  {-# INLINE (***) #-}

instance ArrowChoice (ST s) where
  left f = lift $ \t -> case t of
    (# s, Left x #) -> case unlift f (# s, x #) of
      (# s', x' #) -> (# s', Left x' #)
    (# s, Right y #) -> (# s, Right y #)
  right f = lift $ \t -> case t of
    (# s, Left x #) -> (# s, Left x #)
    (# s, Right y #) -> case unlift f (# s, y #) of
      (# s', y' #) -> (# s', Right y' #)
  f ||| g = lift $ \t -> case t of
    (# s, Left x #) -> unlift f (# s, x #)
    (# s, Right y #) -> unlift g (# s, y #)
  f +++ g = lift $ \t -> case t of
    (# s, Left x #) -> case unlift f (# s, x #) of
      (# s', x' #) -> (# s', Left x' #)
    (# s, Right y #) -> case unlift g (# s, y #) of
      (# s', y' #) -> (# s', Right y' #)
  {-# INLINE left #-}
  {-# INLINE right #-}
  {-# INLINE (+++) #-}
  {-# INLINE (|||) #-}

instance ArrowApply (ST s) where
  app = lift $ \(# s, (f,x) #) -> unlift f (# s, x #)
  {-# INLINE app #-}
