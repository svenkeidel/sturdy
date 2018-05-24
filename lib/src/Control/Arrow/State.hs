{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Arrows #-}
module Control.Arrow.State where

import Prelude hiding (id,(.))
import Control.Category
import Control.Arrow
import Control.Arrow.Utils
import Control.Monad.State

-- | Arrow-based interface to describe stateful computations.
class Arrow c => ArrowState s c | c -> s where
  -- | Retrieves the current state.
  getA :: c () s
  -- | Sets the current state.
  putA :: c s ()

-- | run computation that modifies the current state.
modifyA :: ArrowState s c => c (x,s) s -> c x ()
modifyA f = putA <<< f <<< (id &&& constA getA)

-- | run computation that modifies the current state.
modifyA' :: ArrowState s c => c (s,x) s -> c x ()
modifyA' f = putA <<< f <<< (constA getA &&& id)

instance MonadState s m => ArrowState s (Kleisli m) where
  getA = Kleisli (const get)
  putA = Kleisli put
