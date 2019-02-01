{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Arrows #-}
module Control.Arrow.State where

import           Prelude hiding (id,(.),const)
import qualified Prelude as P
import           Control.Category
import           Control.Arrow
import           Control.Arrow.Utils
import           Control.Monad.State (MonadState)
import qualified Control.Monad.State as M
import           Data.Profunctor

-- | Arrow-based interface to describe stateful computations.
class (Arrow c, Profunctor c) => ArrowState s c | c -> s where
  -- | Retrieves the current state.
  get :: c () s
  -- | Sets the current state.
  put :: c s ()

-- | run computation that modifies the current state.
modify :: ArrowState s c => c (x,s) s -> c x ()
modify f = put <<< f <<< (id &&& const get)

-- | run computation that modifies the current state.
modify' :: ArrowState s c => c (s,x) s -> c x ()
modify' f = put <<< f <<< (const get &&& id)

instance MonadState s m => ArrowState s (Kleisli m) where
  get = Kleisli (P.const M.get)
  put = Kleisli M.put
