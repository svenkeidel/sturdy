{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Fix.Cache where

import Prelude hiding (lookup)

import Control.Arrow
import Control.Arrow.Trans
import Control.Arrow.Transformer.Const
import Control.Arrow.Transformer.Reader
import Control.Arrow.Transformer.State
import Control.Arrow.Transformer.Static
import Control.Arrow.Transformer.Writer

import Data.Profunctor
import Data.Abstract.Stable

class (Arrow c, Profunctor c) => ArrowCache a b c | c -> a, c -> b where
  type Widening c

  -- | Initializes a cache entry with 'bottom'.
  --
  -- The operation satisfies the following laws:
  --  * 'initialize' is an update with bottom: @(_,_,b)<-update-<(Unstable,a,⊥) ⊑ b'<-initialize-<a@
  initialize :: (?cacheWidening :: Widening c) => c a b

  -- | Looks up if there is an entry in the cache.
  lookup :: c a (Maybe (Stable,b))

  -- | Update an existing entry in the cache.
  --
  -- The operation satisfies the following laws:
  --  * 'update' increases the entry: if @(s',a',b') <- update -< (s,a,b)@ then @a ⊑ a'@, and @b ⊑ b'@
  --  * 'update' does not forget: if @(s',a',b') <- update -< (s,a,b); m <- lookup -< a@ then @Just (s ⊔ s',b') ⊑ m@
  update :: (?cacheWidening :: Widening c) => c (Stable,a,b) (Stable,a,b)

  -- | Write a new entry to the cache.
  write :: c (a,b,Stable) ()

  -- | Set a given entry to stable or unstable.
  setStable :: c (Stable,a) ()

  default initialize :: (c ~ t c', ArrowTrans t, ArrowCache a b c', ?cacheWidening :: Widening c') => c a b
  default lookup :: (c ~ t c', ArrowTrans t, ArrowCache a b c') => c a (Maybe (Stable,b))
  default write :: (c ~ t c', ArrowTrans t, ArrowCache a b c') => c (a,b,Stable) ()
  default update :: (c ~ t c', ArrowTrans t, ArrowCache a b c', ?cacheWidening :: Widening c') => c (Stable,a,b) (Stable,a,b)
  default setStable :: (c ~ t c', ArrowTrans t, ArrowCache a b c') => c (Stable,a) ()

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

class (ArrowIterateCache a b c) => ArrowParallelCache a b c where
  lookupOldCache :: (?cacheWidening :: Widening c) => c a b
  lookupNewCache :: c a (Maybe b)
  updateNewCache :: (?cacheWidening :: Widening c) => c (a,b) b
  isStable :: c () Stable

  default lookupOldCache :: (c ~ t c', ArrowTrans t, ArrowParallelCache a b c', ?cacheWidening :: Widening c') => c a b
  default lookupNewCache :: (c ~ t c', ArrowTrans t, ArrowParallelCache a b c') => c a (Maybe b)
  default updateNewCache :: (c ~ t c', ArrowTrans t, ArrowParallelCache a b c', ?cacheWidening :: Widening c') => c (a,b) b
  default isStable :: (c ~ t c', ArrowTrans t, ArrowParallelCache a b c') => c () Stable

  lookupOldCache = lift' lookupOldCache
  lookupNewCache = lift' lookupNewCache
  updateNewCache = lift' updateNewCache
  isStable = lift' isStable

  {-# INLINE lookupOldCache #-}
  {-# INLINE lookupNewCache #-}
  {-# INLINE updateNewCache #-}
  {-# INLINE isStable #-}

class (Arrow c, Profunctor c) => ArrowIterateCache a b c | c -> a, c -> b where
  nextIteration :: c (a,b) (a,b)
  default nextIteration :: (c ~ t c', ArrowTrans t, ArrowIterateCache a b c') => c (a,b) (a,b)
  nextIteration = lift' nextIteration
  {-# INLINE nextIteration #-}

class (Arrow c, Profunctor c) => ArrowGetCache cache c where
  getCache :: c () cache
  default getCache :: (c ~ t c', ArrowTrans t, ArrowGetCache cache c') => c () cache
  getCache = lift' getCache
  {-# INLINE getCache #-}

------------- Instances --------------
instance ArrowCache a b c => ArrowCache a b (ConstT r c) where
  type Widening (ConstT r c) = Widening c

instance ArrowCache a b c => ArrowCache a b (ReaderT r c) where
  type Widening (ReaderT r c) = Widening c

instance ArrowParallelCache a b c => ArrowParallelCache a b (ReaderT r c)
instance ArrowIterateCache a b c => ArrowIterateCache a b (ReaderT r c)
instance ArrowGetCache cache c => ArrowGetCache cache (ReaderT r c)

instance ArrowCache a b c => ArrowCache a b (StateT s c) where
  type Widening (StateT s c) = Widening c

instance ArrowParallelCache a b c => ArrowParallelCache a b (StateT s c)
instance ArrowIterateCache a b c => ArrowIterateCache a b (StateT s c)
instance ArrowGetCache cache c => ArrowGetCache cache (StateT s c)

instance (Applicative f, ArrowCache a b c) => ArrowCache a b (StaticT f c) where
  type Widening (StaticT f c) = Widening c
  {-# SPECIALIZE instance ArrowCache a b c => ArrowCache a b (StaticT ((->) r) c) #-}

instance (Monoid w, ArrowCache a b c) => ArrowCache a b (WriterT w c) where
  type Widening (WriterT w c) = Widening c
