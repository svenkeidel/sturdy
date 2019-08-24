{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Transformer.Abstract.Fix.Stack where

import Prelude hiding (pred,lookup,map,head,iterate,(.))

import Control.Category
import Control.Arrow
import Control.Arrow.Cache
import Control.Arrow.Const
import Control.Arrow.Reader
import Control.Arrow.State
import Control.Arrow.Trans
import Control.Arrow.Order(ArrowJoin(..),ArrowComplete(..),ArrowEffectCommutative)

import Control.Arrow.Transformer.Const
import Control.Arrow.Transformer.Reader
import Control.Arrow.Transformer.State

import Data.Profunctor
import Data.Profunctor.Unsafe((.#))
import Data.Coerce
import Data.Empty
import Data.Order
import Data.Identifiable

import Data.Abstract.StackWidening
import Data.Abstract.Widening(Widening,Stable(..))
import Data.Maybe(fromMaybe)
import Data.HashMap.Lazy(HashMap)
import qualified Data.HashMap.Lazy as M

newtype StackT stack a b c x y = StackT (ConstT (StackWidening stack a, Widening b) (ReaderT (stack a) (StateT (Cache a b) c)) x y)
  deriving (Profunctor,Category,Arrow,ArrowChoice)

runStackT :: (IsEmpty (stack a), Profunctor c)
          => StackWidening stack a -> Widening b -> StackT stack a b c x y -> c x (Cache a b, y)
runStackT stackWiden widen (StackT f) = lmap (\x -> (empty,(empty,x))) (runStateT (runReaderT (runConstT (stackWiden,widen) f)))
{-# INLINE runStackT #-}

newtype Cache a b = Cache (HashMap a (Stable,b))
instance (Show a, Show b) => Show (Cache a b) where
  show (Cache m) = show (M.toList m)

instance IsEmpty (Cache a b) where
  empty = Cache M.empty
  {-# INLINE empty #-}

instance (Identifiable a, LowerBounded b, ArrowChoice c, Profunctor c) => ArrowCache a b (StackT stack a b c) where
  memoize (StackT f) = StackT $ askConst $ \(widen,_) -> proc a -> do
    Cache cache <- get -< ()
    case M.lookup a cache of
      Just (Stable,b) -> f -< (a,Cached (Stable,b))
      _ -> do
        stack <- ask -< ()
        let ((a',l),stack') = widen a stack
        case M.lookup a' cache of
          Just (Stable,b) -> f -< (a',Cached (Stable,b))
          b -> case l of
                 NoLoop -> local f -< (stack',(a',Compute))
                 Loop -> f -< (a',Cached (fromMaybe (Instable,bottom) b))
  write = StackT $ modify' (\((a,b,s),Cache cache) -> ((),Cache (M.insert a (s,b) cache)))
  update = StackT $ askConst $ \(_,widen) -> modify' (\((a,b),Cache cache) ->
    let (_,bOld) = fromMaybe (Instable,bottom) (M.lookup a cache)
        bNew = widen bOld b
    in (bNew,Cache (M.insert a bNew cache)))
  setStable = StackT $ modify' $ \((s,a),Cache cache) -> ((),Cache (M.adjust (first (const s)) a cache))
  {-# INLINE memoize #-}
  {-# INLINE write #-}
  {-# INLINE update #-}
  {-# INLINE setStable #-}

instance (IsEmpty (stack a), ArrowRun c) => ArrowRun (StackT stack a b c) where
  type Run (StackT stack a b c) x y = StackWidening stack a -> Widening b -> Run c x (Cache a b,y)
  run f stackWiden widen = run (runStackT stackWiden widen f)
  {-# INLINE run #-}

instance (Profunctor c,ArrowApply c) => ArrowApply (StackT cache a b c) where
  app = StackT (app .# first coerce)
  {-# INLINE app #-}

instance (Complete y, ArrowEffectCommutative c) => ArrowComplete y (StackT stack a b c) where
  StackT f <⊔> StackT g = StackT $ rmap (uncurry (⊔)) (f &&& g)
  {-# INLINE (<⊔>) #-}

instance (Arrow c, Profunctor c) => ArrowJoin (StackT stack a b c) where
  joinSecond (StackT f) = StackT (second f)
  {-# INLINE joinSecond #-}

instance ArrowEffectCommutative c => ArrowEffectCommutative (StackT stack a b c)
