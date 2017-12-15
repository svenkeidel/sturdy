module Control.Monad.Deduplicate where

import Data.Hashable
import Control.Monad.State
import Control.Monad.Reader

class Monad m => MonadDeduplicate m where
  dedup :: (Hashable a,Eq a) => m a -> m a

instance (MonadDeduplicate m, Hashable s, Eq s) => MonadDeduplicate (StateT s m) where
  dedup = mapStateT dedup

instance MonadDeduplicate m => MonadDeduplicate (ReaderT r m) where
  dedup = mapReaderT dedup
