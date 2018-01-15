{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Monad.Join where

import Control.Arrow
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State (StateT(..))
import Data.Order

class (Complete s,Monad m) => MonadJoin s m | m -> s where
  get :: m s
  get = state (\s -> (s,s))

  put :: s -> m ()
  put s = state (\s' -> ((),s ⊔ s'))

  state :: (s -> (a, s)) -> m a
  state f = do
    s <- get
    let (a,s') = f s
    put s'
    return a

  {-# MINIMAL state | get, put #-}

modify :: MonadJoin s m => (s -> s) -> m ()
modify f = state (\s -> ((), f s))

gets :: MonadJoin s m => (s -> a) -> m a
gets f = do
  s <- get
  return (f s)

newtype JoinT s m a = JoinT {runJoinT :: s -> m (a,s)}

instance Functor m => Functor (JoinT s m) where
  fmap f (JoinT g) = JoinT $ \s -> first f <$> g s

instance Monad m => Applicative (JoinT s m) where
  pure = return
  (<*>) = ap

instance Monad m => Monad (JoinT s m) where
  return x = JoinT $ \s -> return (x,s)
  (JoinT f) >>= k = JoinT $ \s -> do
    (a,s') <- f s
    runJoinT (k a) s'

instance (Complete s, Monad m) => MonadJoin s (JoinT s m) where
  state f = JoinT (\s -> return (f s))

instance MonadTrans (JoinT s) where
  lift ma = JoinT $ \s -> do
    a <- ma
    return (a,s)

mapJoinT :: (m (a, s) -> n (b, s)) -> JoinT s m a -> JoinT s n b
mapJoinT f (JoinT g) = JoinT (\s -> f (g s))

instance MonadReader r m => MonadReader r (JoinT s m) where
  ask = lift ask
  local = mapJoinT . local
  reader = lift . reader

instance MonadJoin j m => MonadJoin j (ReaderT s m) where
  state f = lift (state f)

instance MonadJoin j m => MonadJoin j (StateT s m) where
  state f = lift (state f)
