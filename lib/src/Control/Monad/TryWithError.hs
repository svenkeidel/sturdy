{-# LANGUAGE FlexibleInstances #-}
module Control.Monad.TryWithError where

import Prelude hiding (fail)

import Control.Monad.Reader hiding (fail)
import Control.Monad.State hiding (fail)

class Monad m => MonadTryWithError m where
  tryWithError :: m a -> (a -> m b) -> (String -> m b) -> m b

instance MonadTryWithError m => MonadTryWithError (ReaderT r m) where
  tryWithError (ReaderT f) g h = ReaderT (\r -> tryWithError (f r) (\a -> runReaderT (g a) r) (\e -> runReaderT (h e) r))

instance MonadTryWithError m => MonadTryWithError (StateT s m) where
  tryWithError (StateT f) g h = StateT (\s -> tryWithError (f s) (\(a,s') -> runStateT (g a) s') (\e -> runStateT (h e) s))

instance MonadTryWithError (Either String) where
  tryWithError (Left e) _ z = z e
  tryWithError (Right x) f _ = f x
