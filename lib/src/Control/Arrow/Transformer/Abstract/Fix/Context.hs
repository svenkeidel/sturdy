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
module Control.Arrow.Transformer.Abstract.Fix.Context where

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

import Data.Profunctor(Profunctor(..))
import Data.Empty
import Data.Order

import Data.Abstract.Context
import Data.Abstract.Widening(Widening,Stable(..))
import Data.HashMap.Lazy(HashMap)
import qualified Data.HashMap.Lazy as M

newtype ContextT ctx lab a b c x y = ContextT (ConstT (Widening a, Widening b) (ReaderT ctx (StateT (Cache ctx a b) c)) x y)
  deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowTrans)

newtype Cache ctx a b = Cache (HashMap ctx (a,b,Stable))
instance IsEmpty (Cache ctx a b) where
  empty = Cache M.empty
  {-# INLINE empty #-}

instance (IsContext ctx lab, PreOrd a, Eq a, LowerBounded b, ArrowChoice c, Profunctor c) => ArrowCache (lab,a) b (ContextT ctx lab a b c) where
  memoize (ContextT f) = ContextT $ askConst $ \(widen,_) -> pushCtx $ proc (ctx,(lab,a)) -> do
    Cache cache <- get -< ()
    case M.lookup ctx cache of
      Just (a',b,s)
        | a ⊑ a'    -> f -< ((lab,a),Cached (s,b))
        | otherwise -> do
            let (s',a'') = widen a' a
            put -< Cache (M.insert ctx (a'',b,s ⊔ s') cache)
            f -< ((lab,a''),Compute)
      Nothing -> do
        put -< Cache (M.insert ctx (a,bottom,Instable) cache)
        f -< ((lab,a),Compute)
  update = ContextT $ askConst $ \(_,widen) -> proc ((_,a),b) -> do
    ctx <- ask -< ()
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
  setStable = proc _ -> returnA -< ()
    -- ContextT $ proc (s,_) -> do
    --   ctx <- ask -< ()
    --   Cache cache <- get -< ()
    --   put -< Cache (M.adjust (\(a',b',s') -> (a',b',s ⊔ s')) ctx cache)
  {-# INLINE memoize #-}
  {-# INLINE update #-}
  {-# INLINE write #-}
  {-# INLINE setStable #-}

pushCtx :: (IsContext ctx lab, ArrowReader ctx c) => c (ctx,(lab,a)) y -> c (lab,a) y
pushCtx f = proc (lab,a) -> do
  ctx <- ask -< ()
  let ctx' = push lab ctx
  local f -< (ctx',(ctx',(lab,a)))
{-# INLINE pushCtx #-}

runContextT :: (IsEmpty ctx,Profunctor c) => Widening a -> Widening b -> ContextT ctx lab a b c x y -> c x (Cache ctx a b,y)
runContextT wa wb (ContextT f) = lmap (\x -> (empty,(empty,x))) (runStateT (runReaderT (runConstT (wa,wb) f)))
{-# INLINE runContextT #-}

instance (IsEmpty ctx,ArrowRun c) => ArrowRun (ContextT ctx lab a b c) where
  type Run (ContextT ctx lab a b c) x y = Widening a -> Widening b -> Run c x (Cache ctx a b,y)
  run f wa wb = run (runContextT wa wb f)
  {-# INLINE run #-}

instance (Complete y, ArrowEffectCommutative c) => ArrowComplete y (ContextT ctx lab a b c) where
  ContextT f <⊔> ContextT g = ContextT $ rmap (uncurry (⊔)) (f &&& g)
  {-# INLINE (<⊔>) #-}

instance (Arrow c, Profunctor c) => ArrowJoin (ContextT ctx lab a b c) where
  joinSecond (ContextT f) = ContextT (second f)
  {-# INLINE joinSecond #-}

instance ArrowEffectCommutative c => ArrowEffectCommutative (ContextT ctx lab a b c)
