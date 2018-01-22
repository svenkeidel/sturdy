{-# LANGUAGE FlexibleInstances #-}
module Control.Monad.Try where

import Prelude hiding (fail)

import Control.Monad.Reader hiding (fail)
import Control.Monad.State hiding (fail)

class Monad m => MonadTry m where
  try :: m a -> (a -> m b) -> m b -> m b

instance MonadTry m => MonadTry (ReaderT r m) where
  try (ReaderT f) g (ReaderT h) = ReaderT (\r -> try (f r) (\a -> runReaderT (g a) r) (h r))

instance MonadTry m => MonadTry (StateT s m) where
  try (StateT f) g (StateT h) = StateT (\s -> try (f s) (\(a,s') -> runStateT (g a) s') (h s))

instance MonadTry (Either String) where
  try (Left _) _ z = z
  try (Right x) f _ = f x
