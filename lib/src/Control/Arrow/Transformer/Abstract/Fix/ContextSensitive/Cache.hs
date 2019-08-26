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

import Prelude hiding (pred,lookup,map,head,iterate,(.),truncate)

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

newtype CacheT ctx lab a b c x y = CacheT (ConstT (Widening a, Widening b) (ReaderT (HashSet (lab,a)) (StateT (Cache ctx a b) c)) x y)
  deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowTrans)

newtype Cache ctx a b = Cache (HashMap ctx (a,b,Stable)) deriving (Show)
instance IsEmpty (Cache ctx a b) where
  empty = Cache M.empty
  {-# INLINE empty #-}

instance (Identifiable ctx, Identifiable lab, Identifiable a, PreOrd a, LowerBounded b, ArrowChoice c, Profunctor c)
    => ArrowReuse (ctx,(lab,a)) b (CacheT ctx lab a b c) where
  reuse (CacheT f) = CacheT $ askConst $ \(widen,_) -> proc (ctx,(lab,a)) -> do
    stack <- ask -< ()
    Cache cache <- get -< ()
    (a',b) <- case M.lookup ctx cache of
      Just (a',b,Stable) | a ⊑ a' -> do
        returnA -< (a,Cached (Stable,b))
      Just (a',b,_) -> do
        let (_,a'') = widen a' a
        put -< Cache (M.insert ctx (a'',b,Instable) cache)
        returnA -< (a'',if H.member (lab,a'') stack then Cached (Instable,b) else Compute)
      Nothing -> do
        put -< Cache (M.insert ctx (a,bottom,Instable) cache)
        returnA -< (a,if H.member (lab,a) stack then Cached (Instable,bottom) else Compute)
    local f -< (H.insert (lab,a') stack,((ctx,(lab,a')),b))
  {-# INLINE reuse #-}

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
