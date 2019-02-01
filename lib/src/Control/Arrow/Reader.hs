{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Reader where

import           Control.Arrow
import           Control.Monad.Reader (MonadReader)
import qualified Control.Monad.Reader as M
import           Data.Profunctor

-- | Arrow-based interface for read-only values.
class (Arrow c, Profunctor c) => ArrowReader r c | c -> r where
  -- | Retrieves the current read-only value.
  ask :: c () r
  -- | Runs a computation with a new value.
  local :: c x y -> c (r,x) y

instance MonadReader r m => ArrowReader r (Kleisli m) where
  ask = Kleisli (const M.ask)
  local (Kleisli f) = Kleisli (\(r,x) -> M.local (const r) (f x))
