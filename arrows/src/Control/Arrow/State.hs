{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Arrows #-}
module Control.Arrow.State where

import           Prelude hiding (id,(.),const)
import           Control.Arrow
import           Data.Profunctor

-- | Arrow type class that gives access to a stateful value.
class (Arrow c, Profunctor c) => ArrowState s c | c -> s where

  -- | Retrieves the current state.
  get :: c () s

  -- | Sets the current state.
  put :: c s ()

  -- | Modifies the current state.
  modify :: c (x,s) (y,s) -> c x y
  modify f = proc x -> do
    s <- get -< ()
    (y,s') <- f -< (x,s)
    put -< s'
    returnA -< y
  {-# INLINE modify #-}

-- | Modify the current state with a pure function.
modify' :: ArrowState s c => ((x,s) -> (y,s)) -> c x y
modify' f = modify (arr f)
{-# INLINE modify' #-}
