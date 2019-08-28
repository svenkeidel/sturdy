{-# LANGUAGE Arrows #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Arrow.Cache where

import Control.Arrow
import Data.Profunctor
import Data.Abstract.Widening (Stable)

class (Arrow c, Profunctor c) => ArrowRecurse a b c | c -> a, c -> b where
  -- | Decides whether to return a cached result or to recompute.
  recurse :: c (a,Cached b) y -> c a y

data Cached b = Compute | Cached (Stable,b)
  deriving (Show,Eq)

class (Arrow c, Profunctor c) => ArrowCache a b c | c -> a, c -> b where
  -- | Looks up if there is an entry in the cache.
  lookup :: c a (Maybe (Stable,b))

  -- | Write a new entry to the cache.
  write :: c (a,b,Stable) ()

  -- | Update an existing entry in the cache.
  update :: c (a,b) (Stable,b)

  -- | Set a given entry to stable or unstable.
  setStable :: c (Stable,a) ()

type ArrowCacheRecurse a b c = (ArrowCache a b c, ArrowRecurse a b c)
