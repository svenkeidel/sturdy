{-# LANGUAGE FunctionalDependencies #-}
module Control.Arrow.Fix.Cache where

import Control.Arrow
import Data.Profunctor
import Data.Abstract.Widening

class (Arrow c, Profunctor c) => ArrowCache a b c | c -> a, c -> b where
  -- | Looks up if there is an entry in the cache.
  lookup :: c a (Maybe (Stable,b))

  -- | Write a new entry to the cache.
  write :: c (a,b,Stable) ()

  -- | Update an existing entry in the cache.
  update :: c (a,b) (Stable,b)

  -- | Set a given entry to stable or unstable.
  setStable :: c (Stable,a) ()
