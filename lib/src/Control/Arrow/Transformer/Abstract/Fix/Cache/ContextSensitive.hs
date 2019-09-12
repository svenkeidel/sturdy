{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Transformer.Abstract.Fix.Cache.ContextSensitive
  ( module Control.Arrow.Transformer.Abstract.Fix.Cache
  , Cache(..)
  ) where

import           Prelude hiding (pred,lookup,map,head,iterate,(.),truncate,elem)

import           Control.Arrow
import           Control.Arrow.Const
import           Control.Arrow.State
import           Control.Arrow.Fix.Cache as Cache
import           Control.Arrow.Fix.Context as Context
import           Control.Arrow.Fix.Reuse as Reuse

import           Control.Arrow.Transformer.Abstract.Fix.Cache

import           Data.Identifiable
import           Data.Profunctor.Unsafe
import           Data.Empty
import           Data.Order

import           Data.Abstract.Widening as W
import           Data.Abstract.Stable

import           Data.HashMap.Lazy(HashMap)
import qualified Data.HashMap.Lazy as M

newtype Cache ctx a b = Cache (HashMap ctx (a,b,Stable)) deriving (Show)

instance IsEmpty (Cache ctx a b) where
  empty = Cache M.empty
  {-# INLINE empty #-}

instance (Identifiable ctx, PreOrd a, LowerBounded b, ArrowChoice c, ArrowContext ctx c) => ArrowJoinContext (Cache ctx) a b c where
  type Widening (Cache ctx) a = W.Widening a
  joinContexts' widen f = proc (Cache cache,a) -> do
    ctx <- askContext -< ()
    (f ||| returnA) -< case M.lookup ctx cache of
      -- If there exists a stable cached entry and the actual input is
      -- smaller than the cached input, recurse the cached result.
      Just (a',b,s)
        | a ⊑ a' -> case s of
            Stable   -> Right b
            Unstable -> Left (Cache cache,a')
        | otherwise ->
          -- If there exists the actual input is not smaller than the cached
          -- input, widen the input and recompute.
          let (_,a'') = widen a' a
          in Left (Cache (M.insert ctx (a'',b,Unstable) cache), a'')
      Nothing -> Left (Cache (M.insert ctx (a,bottom,Unstable) cache), a)
  {-# INLINE joinContexts' #-}

instance (Identifiable ctx, PreOrd a, Eq a, Complete b, ArrowChoice c, Profunctor c, ArrowContext ctx c)
    => ArrowCache a b (CacheT (Cache ctx) a b c) where
  lookup = CacheT $ proc a -> do
    ctx <- askContext -< ()
    Cache cache <- get -< ()
    case M.lookup ctx cache of
      Just (a',b,s)
        | a ⊑ a' -> returnA -< Just (s,b)
        | otherwise -> returnA -< Just (Unstable,b)
      Nothing -> returnA -< Nothing
  update = CacheT $ askConst $ \widening -> proc (a,b) -> do
    ctx <- askContext -< ()
    Cache cache <- get -< ()
    case M.lookup ctx cache of
      Just (a',b',_) -> do
        let (s,b'') = widening b' b
        put -< Cache (M.insert ctx (a',b'',if a == a' then s else Unstable) cache)
        returnA -< (s,b'')
      Nothing -> do
        put -< Cache (M.insert ctx (a,b,Unstable) cache)
        returnA -< (Unstable,b)
  write = CacheT $ proc (a,b,s) -> do
    ctx <- askContext -< ()
    Cache cache <- get -< ()
    case M.lookup ctx cache of
      Just (a',b',s') -> do
        let b'' = b ⊔ b'
        put -< Cache (M.insert ctx (a',b'',if a == a' then s else s') cache)
      Nothing ->
        put -< Cache (M.insert ctx (a,b,s) cache)
  setStable = CacheT $ proc (s,a) -> do
    Cache cache <- get -< ()
    ctx <- askContext -< ()
    put -< Cache (M.adjust (\(a',b',s') -> (a',b',if a == a' then s else s')) ctx cache)
  {-# INLINE lookup #-}
  {-# INLINE update #-}
  {-# INLINE write #-}
  {-# INLINE setStable #-}

instance (PreOrd a, Arrow c, Profunctor c) => ArrowReuse a b (CacheT (Cache ctx) a b c) where
  type Dom (CacheT (Cache ctx) a b c) = a
  reuse f = CacheT $ proc (a,s) -> do
    Cache cache <- get -< ()
    returnA -< M.foldl' (\m (a',b',s') -> if s' ⊑ s && a ⊑ a' then m <> f a a' s' b' else m) mempty cache
  {-# INLINE reuse #-}
