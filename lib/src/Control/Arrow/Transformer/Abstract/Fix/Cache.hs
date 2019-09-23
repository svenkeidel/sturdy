{-# LANGUAGE Arrows #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Arrow.Transformer.Abstract.Fix.Cache where

import Prelude hiding (pred,lookup,map,head,iterate,(.),truncate,elem)

import Control.Category
import Control.Arrow
import Control.Arrow.Const
import Control.Arrow.Trans
import Control.Arrow.State
import Control.Arrow.Fix.Context as Context
import Control.Arrow.Fix.Cache as Cache
import Control.Arrow.Fix.Reuse as Reuse
import Control.Arrow.Order(ArrowJoin(..),ArrowComplete(..),ArrowEffectCommutative)

import Control.Arrow.Transformer.Const
import Control.Arrow.Transformer.State

import Data.Profunctor.Unsafe
import Data.Empty
import Data.Order
import Data.Coerce
import Data.Identifiable
import Data.HashMap.Lazy(HashMap)
import qualified Data.HashMap.Lazy as M
import Data.Monoidal
import Data.Maybe

import Data.Abstract.Stable
import Data.Abstract.Widening as W

import GHC.Exts

newtype CacheT cache a b c x y = CacheT { unCacheT :: ConstT (W.Widening b) (StateT (cache a b) c) x y}
  deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowTrans,ArrowState (cache a b))

runCacheT :: (IsEmpty (cache a b), Profunctor c) => W.Widening b -> CacheT cache a b c x y -> c x (cache a b,y)
runCacheT widen (CacheT f) = lmap (empty,) (runStateT (runConstT widen f))
{-# INLINE runCacheT #-}

liftCacheT :: Arrow c => CacheT cache' a' b c x y -> CacheT cache a b c (cache' a' b,x) (cache' a' b,y)
liftCacheT (CacheT f) = CacheT (lift $ \widen -> withStateT (runConstT widen f))
{-# INLINE liftCacheT #-}

liftCacheT' :: Arrow c => CacheT cache' a' b c x y -> ConstT (W.Widening b) (StateT (cache a b) c) (cache' a' b,x) (cache' a' b,y)
liftCacheT' = coerce liftCacheT
{-# INLINE liftCacheT' #-}

instance (IsEmpty (cache a b), ArrowRun c) => ArrowRun (CacheT cache a b c) where
  type Run (CacheT cache a b c) x y = W.Widening b -> Run c x (cache a b,y)
  run f widen = run (runCacheT widen f)
  {-# INLINE run #-}

instance (Complete y, ArrowEffectCommutative c) => ArrowComplete y (CacheT cache a b c) where
  CacheT f <⊔> CacheT g = CacheT $ rmap (uncurry (⊔)) (f &&& g)
  {-# INLINE (<⊔>) #-}

instance (Arrow c, Profunctor c) => ArrowJoin (CacheT cache a b c) where
  joinSecond (CacheT f) = CacheT (second f)
  {-# INLINE joinSecond #-}

instance (Profunctor c,ArrowApply c) => ArrowApply (CacheT cache a b c) where
  app = CacheT (app .# first coerce)
  {-# INLINE app #-}

instance ArrowEffectCommutative c => ArrowEffectCommutative (CacheT cache a b c)

----- Basic Cache -----
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
        put -< Cache (M.insert a (Unstable,b) cache)
        returnA -< (Unstable,b)
  write = CacheT $ modify' (\((a,b,s),Cache cache) -> ((),Cache (M.insert a (s,b) cache)))
  setStable = CacheT $ modify' $ \((s,a),Cache cache) -> ((),Cache (M.adjust (first (const s)) a cache))
  {-# INLINE lookup #-}
  {-# INLINE write #-}
  {-# INLINE update #-}
  {-# INLINE setStable #-}

instance (Arrow c, ArrowContext ctx a c) => ArrowContext ctx a (CacheT Cache a b c) where
  type Widening (CacheT Cache a b c) a = Context.Widening c a
  askContext = CacheT askContext
  localContext (CacheT f) = CacheT (localContext f)
  joinByContext widen = CacheT $ joinByContext widen
  {-# INLINE askContext #-}
  {-# INLINE localContext #-}
  {-# INLINE joinByContext #-}

instance (PreOrd a, Arrow c, Profunctor c) => ArrowReuse a b (CacheT Cache a b c) where
  reuse f = CacheT $ proc (a,s) -> do
    Cache cache <- get -< ()
    returnA -< M.foldlWithKey' (\m a' (s',b') -> if s' ⊑ s && a ⊑ a' then m <> f a a' s' b' else m) mempty cache
  {-# INLINE reuse #-}


------ Group Cache ------
data Group cache a b where
  Groups :: HashMap k (cache a b) -> Group cache (k,a) b

instance IsEmpty (Group cache (k,a) b) where
  empty = Groups empty
  {-# INLINE empty #-}

instance (Identifiable k, Arrow c, Profunctor c, ArrowCache a b (CacheT cache a b c), IsEmpty (cache a b)) => ArrowCache (k,a) b (CacheT (Group cache) (k,a) b c) where
  lookup = withCache Cache.lookup
  update = lmap assoc2 (withCache Cache.update)
  write = lmap (\((k,a),b,s) -> (k,(a,b,s))) (withCache Cache.write)
  setStable = lmap shuffle1 (withCache Cache.setStable)
  {-# INLINE lookup #-}
  {-# INLINE update #-}
  {-# INLINE write #-}
  {-# INLINE setStable #-}

instance (Arrow c, ArrowContext ctx a c) => ArrowContext ctx (k,a) (CacheT (Group cache) (k,a) b c) where
  type Widening (CacheT (Group cache) (k,a) b c) (k,a) = Context.Widening c a
  askContext = CacheT askContext
  localContext (CacheT f) = CacheT (localContext f)
  joinByContext widen = CacheT $ second (joinByContext widen)
  {-# INLINE askContext #-}
  {-# INLINE localContext #-}
  {-# INLINE joinByContext #-}

instance (Identifiable k, IsEmpty (cache a b), ArrowApply c, Profunctor c, ArrowReuse a b (CacheT cache a b c)) => ArrowReuse (k,a) b (CacheT (Group cache) (k,a) b c) where
  reuse f = proc ((k,a0),s) -> withCache (reuse (\a a' -> f (k,a) (k,a'))) -<< (k,(a0,s))
  {-# INLINE reuse #-}

withCache :: (Identifiable k, IsEmpty (cache a b), Arrow c, Profunctor c) => CacheT cache a b c x y -> CacheT (Group cache) (k,a) b c (k,x) y
withCache f = CacheT $ modify $ proc ((k,x),g) -> do
  let Groups groups = g
  (cache',y) <- liftCacheT' f -< (fromMaybe empty (M.lookup k groups),x)
  returnA -< (y,Groups (M.insert k cache' groups))
{-# INLINE withCache #-}

instance Identifiable k => IsList (Group cache (k,a) b) where
  type Item (Group cache (k,a) b) = (k,cache a b)
  toList (Groups m) = M.toList m
  fromList l = Groups $ M.fromList l

instance (Show k, Show (cache a b)) => Show (Group cache (k,a) b) where
  show (Groups m) = show (M.toList m)
