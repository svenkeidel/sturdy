module Control.Monad.Try where

import Prelude hiding (fail)

import Control.Monad.Reader hiding (fail)
import Control.Monad.State hiding (fail)

class Monad m => MonadTry m where
  fail :: m a
  try :: m a -> (a -> m b) -> m b -> m b

instance MonadTry m => MonadTry (ReaderT r m) where
  fail = ReaderT (const fail)
  try (ReaderT f) g (ReaderT h) = ReaderT (\r -> try (f r) (\a -> runReaderT (g a) r) (h r))

instance MonadTry m => MonadTry (StateT s m) where
  fail = StateT (const fail)
  try (StateT f) g (StateT h) = StateT (\s -> try (f s) (\(a,s') -> runStateT (g a) s') (h s))
