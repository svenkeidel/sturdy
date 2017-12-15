{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Monad.Result where

import           Prelude hiding (fail)
import           Control.Applicative
import           Control.Monad hiding (fail)
import           Control.Monad.Trans
import           Control.Monad.State hiding (fail)
import           Control.Monad.Reader hiding (fail)
import           Control.Monad.Deduplicate
import           Control.Monad.Try
import           Control.Monad.Join (MonadJoin)
import qualified Control.Monad.Join as J

import           Data.Result
import           Data.Order

newtype ResultT m a = ResultT { runResultT :: m (Result a) }

mapResultT :: (m (Result a) -> n (Result b)) -> ResultT m a -> ResultT n b
mapResultT f (ResultT r) = ResultT (f r)

instance Functor m => Functor (ResultT m) where
  fmap f = mapResultT (fmap (fmap f))

instance Monad m => Applicative (ResultT m) where
  pure = return
  (<*>) = ap

instance Monad m => Monad (ResultT m) where
  return x = ResultT (return (return x))
  (ResultT f) >>= k = ResultT $ do
    r <- f
    case r of
      Success a -> runResultT $ k a
      Fail -> return Fail

instance MonadTrans ResultT where
  lift = ResultT . fmap Success

instance MonadState s m => MonadState s (ResultT m) where
  state f = lift (state f)

instance MonadReader r m => MonadReader r (ResultT m) where
  ask = lift ask
  local = mapResultT . local

instance Monad m => MonadTry (ResultT m) where
  fail = ResultT (return fail)
  try (ResultT f) g (ResultT h) = ResultT $ do
    r <- f
    case r of
      Success b -> runResultT $ g b
      Fail -> h

instance MonadPlus m => Alternative (ResultT m) where
  empty = mzero
  (<|>) = mplus

instance MonadPlus m => MonadPlus (ResultT m) where
  mzero = lift mzero
  mplus (ResultT f) (ResultT g) = ResultT (f `mplus` g)

instance MonadJoin j m => MonadJoin j (ResultT m) where
  state f = lift (J.state f)

instance MonadDeduplicate m => MonadDeduplicate (ResultT m) where
  dedup = mapResultT dedup

instance PreOrd (m (Result a)) => PreOrd (ResultT m a) where
  (ResultT m1) ⊑ (ResultT m2) = m1 ⊑ m2

instance PartOrd (m (Result a)) => PartOrd (ResultT m a) where

instance Lattice (m (Result a)) => Lattice (ResultT m a) where
  (ResultT m1) ⊔ (ResultT m2) = ResultT (m1 ⊔ m2)

