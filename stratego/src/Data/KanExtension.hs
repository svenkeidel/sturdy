{-# LANGUAGE FlexibleContexts #-}
module Data.KanExtension where

import Control.Monad.State

import           Data.Hashable (Hashable)
import           Data.HashSet (HashSet)
import qualified Data.HashSet as H
import           Data.HashMap.Lazy(HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.Order

-- Left Kan Extension of f along g.
data KanExtension a b c =
  KanExtension
  { f :: a -> c,
    g :: a -> b,
    l :: HashMap b c
  } 

kanExtension :: (Eq b, Hashable b, Lattice a, Lattice b, Lattice c) => (a -> c) -> (a -> b) -> KanExtension a b c
kanExtension f0 g0 = KanExtension f0 g0 mempty

apply :: (MonadState (KanExtension a b c) m, Eq a, Hashable a, Eq b, Hashable b, Lattice a, Lattice b, Lattice c)
      => a -> m c
apply a = do
  k <- get
  let b = g k a
  case M.lookup b (l k) of
    Just c | otherwise -> do
      let c' = f k a ⊔ c
      put (k {l = M.insert b c' (l k)})
      return c'
    Nothing -> do
      let c = f k a
      put (k {l = M.insert b c (l k)})
      return c
