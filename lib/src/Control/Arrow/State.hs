{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Arrows #-}
module Control.Arrow.State where

import           Prelude hiding (id,(.),const)
import           Control.Arrow
import           Data.Profunctor

-- | Arrow-based interface to describe stateful computations.
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

modify' :: ArrowState s c => ((x,s) -> (y,s)) -> c x y
modify' f = modify (arr f)
