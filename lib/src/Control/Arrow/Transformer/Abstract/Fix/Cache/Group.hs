{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Transformer.Abstract.Fix.Cache.Group where

import Prelude hiding (pred,lookup,map,head,iterate,(.))

import Control.Arrow
import Control.Arrow.Fix.Reuse
import Control.Arrow.Fix.Context
import Control.Arrow.Fix.Cache as Cache
import Control.Arrow.State

import Control.Arrow.Transformer.Abstract.Fix.Cache
import Control.Arrow.Transformer.Reader

import Data.Profunctor
import Data.Identifiable
import Data.Empty
import Data.HashMap.Lazy(HashMap)
import qualified Data.HashMap.Lazy as M
import Data.Maybe (fromMaybe)

import Data.Monoidal

data Group cache a b where
  Groups :: HashMap k (cache a b) -> Group cache (k,a) b

instance (Show k, Show (cache a b)) => Show (Group cache (k,a) b) where
  show (Groups m) = show (M.toList m)

instance IsEmpty (Group cache (k,a) b) where
  empty = Groups empty
  {-# INLINE empty #-}

instance (Identifiable k, IsEmpty (cache a b), Arrow c, ArrowJoinContext cache a b (ReaderT (k,Group cache (k,a) b) c)) => ArrowJoinContext (Group cache) (k,a) b c where
  type Widening (Group cache) (k,a) = Widening cache a
  joinContexts' widen f = proc (g,(k,a)) -> do
    let Groups groups = g
    runReaderT (joinContexts' widen (ReaderT (proc ((k,g),(cache,a)) -> do
        let Groups groups = g
        f -< (Groups (M.insert k cache groups),(k,a))
      ))) -< ((k,g),(fromMaybe empty (M.lookup k groups),a))
  {-# INLINE joinContexts' #-}

instance (Identifiable k, Arrow c, Profunctor c, ArrowCache a b (CacheT cache a b c), IsEmpty (cache a b)) => ArrowCache (k,a) b (CacheT (Group cache) (k,a) b c) where
  lookup = withCache Cache.lookup
  update = lmap assoc2 (withCache Cache.update)
  write = lmap (\((k,a),b,s) -> (k,(a,b,s))) (withCache Cache.write)
  setStable = lmap shuffle1 (withCache Cache.setStable)
  {-# INLINE lookup #-}
  {-# INLINE update #-}
  {-# INLINE write #-}
  {-# INLINE setStable #-}

instance (Identifiable k, IsEmpty (cache a b), Arrow c, Profunctor c, ArrowReuse a b (CacheT cache a b c)) => ArrowReuse (k,a) b (CacheT (Group cache) (k,a) b c) where
  type Dom (CacheT (Group cache) (k,a) b c) = Dom (CacheT cache a b c)
  reuse f = lmap (\((k,a),s) -> (k,(a,s))) (withCache (reuse f))
  {-# INLINE reuse #-}

withCache :: (Identifiable k, IsEmpty (cache a b), Arrow c, Profunctor c) => CacheT cache a b c x y -> CacheT (Group cache) (k,a) b c (k,x) y
withCache f = CacheT $ modify $ proc ((k,x),g) -> do
  let Groups groups = g
  (cache',y) <- liftCacheT' f -< (fromMaybe empty (M.lookup k groups),x)
  returnA -< (y,Groups (M.insert k cache' groups))
{-# INLINE withCache #-}
