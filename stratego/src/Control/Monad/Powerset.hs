{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Monad.Powerset where

import           Data.Powerset

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.State
import           Control.Monad.Reader
import           Control.Monad.Deduplicate
import           Control.Monad.Join (MonadJoin)
import qualified Control.Monad.Join as J

newtype PowT m a = PowT { runPowT :: m (Pow a) }

mapPowT :: (m (Pow a) -> n (Pow b)) -> PowT m a -> PowT n b
mapPowT f (PowT p) = PowT (f p)
{-# INLINE mapPowT #-}

instance Functor m => Functor (PowT m) where
  fmap f = mapPowT (fmap (fmap f))

instance Monad m => Applicative (PowT m) where
  pure = return
  (<*>) = ap

instance Monad m => Monad (PowT m) where
  return x = PowT (return (return x))
  (PowT f) >>= k = PowT $ do
    r <- f
    join <$> mapM (runPowT . k) r

instance MonadTrans PowT where
  lift = PowT . fmap return

instance MonadState s m => MonadState s (PowT m) where
  state f = lift (state f)

instance MonadReader r m => MonadReader r (PowT m) where
  ask = lift ask
  local = mapPowT . local

instance Monad m => Alternative (PowT m) where
  empty = mzero
  (<|>) = mplus

instance Monad m => MonadPlus (PowT m) where
  mzero = PowT $ return mzero
  mplus (PowT f) (PowT g) = PowT $ mplus <$> f <*> g

instance MonadJoin j m => MonadJoin j (PowT m) where
  state f = lift (J.state f)

instance Monad m => MonadDeduplicate (PowT m) where
  dedup = mapPowT (fmap dedup)
