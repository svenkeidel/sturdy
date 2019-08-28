{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Arrow.Transformer.Abstract.Fix.ContextSensitive.Cache where

import Prelude hiding (pred,lookup,map,head,iterate,(.),truncate,elem)

import Control.Category
import Control.Arrow
import Control.Arrow.Trans
import Control.Arrow.Const
import Control.Arrow.Reader
import Control.Arrow.State
import Control.Arrow.Cache as Cache
import Control.Arrow.Order(ArrowJoin(..),ArrowComplete(..),ArrowEffectCommutative)

import Control.Arrow.Transformer.Const
import Control.Arrow.Transformer.Reader
import Control.Arrow.Transformer.State

import Data.Identifiable
import Data.Profunctor.Unsafe
import Data.Empty
import Data.Order
import Data.Coerce

import Data.Abstract.Widening(Widening,Stable(..))

import           Data.HashSet(HashSet)
import qualified Data.HashSet as H
import           Data.HashMap.Lazy(HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.Maybe(fromMaybe)

newtype CacheT ctx lab a b c x y = CacheT
  (ConstT (Widening a, Widening b)
    (ReaderT (Stack lab a)
      (StateT (Cache ctx a b) c)) x y)
  deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowTrans)

newtype Cache ctx a b = Cache (HashMap ctx (a,b,Stable)) deriving (Show)

instance IsEmpty (Cache ctx a b) where
  empty = Cache M.empty
  {-# INLINE empty #-}

instance (Identifiable ctx, Identifiable lab, Identifiable a, PreOrd a, LowerBounded b, ArrowChoice c, Profunctor c)
    => ArrowRecurse (ctx,(lab,a)) b (CacheT ctx lab a b c) where
  recurse (CacheT f) = CacheT $ askConst $ \(widen,_) -> proc (ctx,(lab,a)) -> do
    Cache cache <- get -< ()
    (a',b) <- case M.lookup ctx cache of
      -- If there exists a stable cached entry and the actual input is
      -- smaller than the cached input, recurse the cached result.
      Just (a',b,Stable) | a ⊑ a' -> do
        returnA -< (a,Cached (Stable,b))

      Just (a',b,_) -> do
        -- If there exists an unstable cached entry or the actual input is
        -- not smaller than the cached input, widen the input and recompute.
        let (_,a'') = widen a' a
        put -< Cache (M.insert ctx (a'',b,Instable) cache)

        -- If the stack already contains the call, return the instable
        -- cached result to avoid divergence.
        stack <- ask -< ()
        returnA -< if elem lab a'' stack
                   then (a'',Cached (Instable,b))
                   else (a'',Compute)

      Nothing -> do
        put -< Cache (M.insert ctx (a,bottom,Instable) cache)

        stack <- ask -< ()
        returnA -< if elem lab a stack
                   then (a,Cached (Instable,bottom))
                   else (a,Compute)

    -- Finally, push the new call on the stack.
    stack <- ask -< ()
    local f -< (push lab a' stack,((ctx,(lab,a')),b))
  {-# INLINE recurse #-}

instance (Identifiable ctx, PreOrd a, Eq a, LowerBounded b, ArrowChoice c, Profunctor c) => ArrowCache (ctx,(lab,a)) b (CacheT ctx lab a b c) where
  lookup = CacheT $ proc (ctx,(_,a)) -> do
    Cache cache <- get -< ()
    case M.lookup ctx cache of
      Just (a',b,s)
        | a ⊑ a' -> returnA -< Just (s,b)
        | otherwise -> returnA -< Just (Instable,b)
      Nothing -> returnA -< Nothing
  update = CacheT $ askConst $ \(_,widen) -> proc ((ctx,(_,a)),b) -> do
    Cache cache <- get -< ()
    case M.lookup ctx cache of
      Just (a',b',_) -> do
        let (s,b'') = widen b' b
        put -< Cache (M.insert ctx (a',b'',s) cache)
        returnA -< (s,b'')
      Nothing -> do
        put -< Cache (M.insert ctx (a,b,Instable) cache)
        returnA -< (Instable,b)
  write = proc (a,b,s) -> do
    update -< (a,b)
    setStable -< (s,a)
  setStable = CacheT $ proc (s,(ctx,(_,a))) -> do
    Cache cache <- get -< ()
    put -< Cache (M.adjust (\(a',b',s') -> (a',b',if a == a' then s else s')) ctx cache)
  {-# INLINE lookup #-}
  {-# INLINE update #-}
  {-# INLINE write #-}
  {-# INLINE setStable #-}

newtype Stack lab a = Stack (HashMap lab (HashSet a))

instance IsEmpty (Stack lab a) where
  empty = Stack M.empty
  {-# INLINE empty #-}

push :: (Identifiable lab, Identifiable a) => lab -> a -> Stack lab a -> Stack lab a
push lab a (Stack s) = Stack (M.insertWith (\_ old -> H.insert a old) lab (H.singleton a) s)
{-# INLINE push #-}

elem :: (Identifiable lab, Identifiable a) => lab -> a -> Stack lab a -> Bool
elem lab a (Stack s) = fromMaybe False $ do
  as <- M.lookup lab s
  return (H.member a as)
{-# INLINE elem #-}

runCacheT :: Profunctor c => Widening a -> Widening b -> CacheT ctx lab a b c x y -> c x (Cache ctx a b,y)
runCacheT wa wb (CacheT f) = lmap (\x -> (empty,(empty,x))) (runStateT (runReaderT (runConstT (wa,wb) f)))
{-# INLINE runCacheT #-}

instance ArrowRun c => ArrowRun (CacheT ctx lab a b c) where
  type Run (CacheT ctx lab a b c) x y = Widening a -> Widening b -> Run c x (Cache ctx a b,y)
  run f wa wb = run (runCacheT wa wb f)
  {-# INLINE run #-}

instance (Complete y, ArrowEffectCommutative c) => ArrowComplete y (CacheT ctx lab a b c) where
  CacheT f <⊔> CacheT g = CacheT $ rmap (uncurry (⊔)) (f &&& g)
  {-# INLINE (<⊔>) #-}

instance (Arrow c, Profunctor c) => ArrowJoin (CacheT ctx lab a b c) where
  joinSecond (CacheT f) = CacheT (second f)
  {-# INLINE joinSecond #-}

instance (Profunctor c,ArrowApply c) => ArrowApply (CacheT ctx lab a b c) where
  app = CacheT (app .# first coerce)
  {-# INLINE app #-}

instance ArrowEffectCommutative c => ArrowEffectCommutative (CacheT ctx lab a b c)
