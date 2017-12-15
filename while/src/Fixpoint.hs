{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Fixpoint where

import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.FreeCoCompletion
import           Data.FreeCompletion
import qualified Data.List as L
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe
import           Data.Order
import           Text.Printf

newtype Cache a b = Cache (Map a b) deriving (Eq)

instance (Show a, Show b) => Show (Cache a b) where
  show (Cache m) = show (M.toList m)

memoize :: (Ord a, Logging a b m, LowerBounded b, Complete b, MonadReader (Cache a b) m, MonadState (Cache a b) m)
        => (a -> m b) -> (a -> m b)
memoize f a = do
  Cache outCache <- get
  case M.lookup a outCache of
    Just b -> do
      tell [printf "Cache hit: f(%s) = %s" (show a) (show b)]
      return b
    Nothing -> do
      tell [printf "Cache miss: compute f(%s)" (show a)]
      Cache inCache <- ask
      let b0 = fromMaybe bottom (M.lookup a inCache)
      put (Cache (M.insert a b0 outCache))
      b <- f a
      modifyCache (M.insertWith (⊔) a b)
      return b

fillCache :: (Ord a, Logging a b m, PreOrd b, MonadReader (Cache a b) m, MonadState (Cache a b) m)
          => (a -> m b) -> (a -> m b)
fillCache f a = loop
  where
    loop = do
      oldCache <- get
      put bottom
      b <- local (const oldCache) (f a)
      newCache <- get
      if oldCache ≈ newCache
      then do
        tell [printf "Final cache:\n%s" (show newCache)]
        return b
      else do
        tell [printf "Cache grew and the following elements where added:\n%s" (show (cacheDifference oldCache newCache))]
        loop

type Int' = FreeCoCompletion (FreeCompletion Int)
 
fib :: Monad m => (Int -> m Int') -> (Int -> m Int')
fib _ 0 = return 0
fib _ 1 = return 1
fib fib' n = (+) <$> fib' (n-2) <*> fib' (n-1)

plusOneForever :: Monad m => (Int -> m Int') -> (Int -> m Int')
plusOneForever r n = r (n + 1)
                     
omega :: (Int -> m Int') -> (Int -> m Int')
omega r = r

test :: (Ord a, Show a, Show b, LowerBounded b, Complete b)
     => (forall m. (Logging a b m, MonadReader (Cache a b) m, MonadState (Cache a b) m) => (a -> m b) -> (a -> m b)) -> a -> [String]
test f a = execWriter $ runReaderT (runStateT (fillCache (fix (f . memoize)) a) bottom) bottom

modifyCache :: (Ord a, MonadState (Cache a b) m) => (Map a b -> Map a b) -> m ()
modifyCache f = modify (\(Cache c) -> Cache (f c))

cacheDifference :: Ord a => Cache a b -> Cache a b -> [(a,b)]
cacheDifference (Cache old) (Cache new) = M.toList (M.difference new old)

type Logging a b m = (Show a,Show b, MonadWriter [String] m)

instance (Eq a,Ord a,PreOrd b) => PreOrd (Cache a b) where
  Cache c1 ⊑ Cache c2 = M.keys c1 ⊆ M.keys c2 && all (\k -> (c1 M.! k) ⊑ (c2 M.! k)) (M.keys c1)
    where (⊆) = L.isSubsequenceOf
  Cache c1 ≈ Cache c2 = M.keys c1 == M.keys c2 && all (\k -> (c1 M.! k) ≈ (c2 M.! k)) (M.keys c1)
-- instance (Eq a,Ord a,PartOrd b) => PartOrd (Cache a b) where

instance (Eq a,Ord a,Complete b) => Complete (Cache a b) where
  Cache c1 ⊔ Cache c2 = Cache (M.unionWith (⊔) c1 c2)

instance (Eq a,Ord a,PreOrd b) => LowerBounded (Cache a b) where
  bottom = Cache M.empty
