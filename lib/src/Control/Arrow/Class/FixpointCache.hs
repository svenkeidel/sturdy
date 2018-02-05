{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Class.FixpointCache(ArrowCache(..),fix) where

import           Prelude hiding (id,(.))
import qualified Data.Function as F

import           Control.Arrow hiding (loop)
import           Control.Category

class Arrow c => ArrowCache x y c | c -> x, c -> y where
  -- | Retrieves a value from the cache.
  askCache :: c x (Maybe y)

  -- | Initialize the cache with the value from the previous fixpoint iteration. If it does not exist, set it to bottom.
  initializeCache :: c x ()

  -- | Update the cache by taking the least upper bound of the old value with the new value.
  updateCache :: c (x,y) ()

  -- | Replaces the cache of the previous fixpoint iteration with a new cache.
  retireCache :: c x y -> c x y
                  
  -- | The fixpoint is reached if the outcache after running the computation is equal to the incache.
  reachedFixpoint :: c () Bool

memoize :: (ArrowChoice c, ArrowCache x y c) => c x y -> c x y
memoize f = proc x -> do
  m <- askCache -< x
  case m of
    Just y -> returnA -< y
    Nothing -> do
      initializeCache -< x
      y <- f -< x
      updateCache -< (x,y)
      returnA -< y

-- | Specialized fixpoint algorithm 
fix :: (ArrowChoice c, ArrowCache x y c) => (c x y -> c x y) -> c x y
fix f = proc x -> do
  y <- retireCache (F.fix (f . memoize)) -< x
  fp <- reachedFixpoint -< ()
  if fp
  then returnA -< y
  else fix f -< x
