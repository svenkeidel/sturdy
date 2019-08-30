{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Transformer.Abstract.Fix.Cache.Basic where

import Prelude hiding (pred,lookup,map,head,iterate,(.))

import Control.Arrow
import Control.Arrow.Fix.Reuse
import Control.Arrow.Fix.Cache
import Control.Arrow.State
import Control.Arrow.Const

import Control.Arrow.Transformer.Abstract.Fix.Cache

import Data.Order
import Data.Profunctor
import Data.Identifiable
import Data.Empty
import Data.HashMap.Lazy(HashMap)
import qualified Data.HashMap.Lazy as M

import Data.Abstract.Widening(Stable(..))

newtype Cache a b = Cache { getMap :: HashMap a (Stable,b)}
instance (Show a, Show b) => Show (Cache a b) where
  show (Cache m) = show (M.toList m)

instance IsEmpty (Cache a b) where
  empty = Cache M.empty
  {-# INLINE empty #-}

instance (Identifiable a, ArrowChoice c, Profunctor c) => ArrowCache a b (CacheT Cache a b c) where
  lookup = CacheT $ proc a -> do
    Cache cache <- get -< ()
    returnA -< M.lookup a cache
  update = CacheT $ askConst $ \widen -> proc (a,b) -> do
    Cache cache <- get -< ()
    case M.lookup a cache of
      Just (_,b') -> do
        let b'' = widen b' b
        put -< Cache (M.insert a b'' cache)
        returnA -< b''
      Nothing -> do
        put -< Cache (M.insert a (Instable,b) cache)
        returnA -< (Instable,b)
  write = CacheT $ modify' (\((a,b,s),Cache cache) -> ((),Cache (M.insert a (s,b) cache)))
  setStable = CacheT $ modify' $ \((s,a),Cache cache) -> ((),Cache (M.adjust (first (const s)) a cache))
  {-# INLINE lookup #-}
  {-# INLINE write #-}
  {-# INLINE update #-}
  {-# INLINE setStable #-}

instance (PreOrd a, Arrow c, Profunctor c) => ArrowReuse a b (CacheT Cache a b c) where
  reuseStable f = CacheT $ proc a -> do
    Cache cache <- get -< ()
    returnA -< M.foldlWithKey' (\m a' (s,b) -> if s == Stable && a ⊑ a' then m <> f a a' b else m) mempty cache
  {-# INLINE reuseStable #-}
