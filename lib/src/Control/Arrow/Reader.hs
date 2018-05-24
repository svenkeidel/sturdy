{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Reader where

import Control.Arrow
import Control.Monad.Reader

-- | Arrow-based interface for read-only values.
class Arrow c => ArrowReader r c | c -> r where
  -- | Retrieves the current read-only value.
  askA :: c () r
  -- | Runs a computation with a new value.
  localA :: c x y -> c (r,x) y

instance MonadReader r m => ArrowReader r (Kleisli m) where
  askA = Kleisli (const ask)
  localA (Kleisli f) = Kleisli (\(r,x) -> local (const r) (f x))
