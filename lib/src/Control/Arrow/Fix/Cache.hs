{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
module Control.Arrow.Fix.Cache where

import Prelude hiding (lookup)
import Control.Arrow
import Control.Arrow.Trans
import Data.Profunctor
import Data.Abstract.Stable

class (Arrow c, Profunctor c) => ArrowCache a b c | c -> a, c -> b where
  -- | Initializes a cache entry with 'bottom'.
  initialize :: c a b

  -- | Looks up if there is an entry in the cache.
  lookup :: c a (Maybe (Stable,b))

  -- | Write a new entry to the cache.
  write :: c (a,b,Stable) ()

  -- | Update an existing entry in the cache.
  update :: c (a,b) (Stable,b)

  -- | Set a given entry to stable or unstable.
  setStable :: c (Stable,a) ()

  default initialize :: (c ~ t c', ArrowLift t, ArrowCache a b c') => c a b
  default lookup :: (c ~ t c', ArrowLift t, ArrowCache a b c') => c a (Maybe (Stable,b))
  default write :: (c ~ t c', ArrowLift t, ArrowCache a b c') => c (a,b,Stable) ()
  default update :: (c ~ t c', ArrowLift t, ArrowCache a b c') => c (a,b) (Stable,b)
  default setStable :: (c ~ t c', ArrowLift t, ArrowCache a b c') => c (Stable,a) ()

  initialize = lift' initialize
  lookup = lift' lookup
  write = lift' write
  update = lift' update
  setStable = lift' setStable

  {-# INLINE initialize #-}
  {-# INLINE lookup #-}
  {-# INLINE write #-}
  {-# INLINE update #-}
  {-# INLINE setStable #-}
