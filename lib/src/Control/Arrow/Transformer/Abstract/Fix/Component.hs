{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Control.Arrow.Transformer.Abstract.Fix.Component(ComponentT,runComponentT,Component,Proj2) where

import           Prelude hiding (id,pred,lookup,map,head,iterate,(.),elem)

import           Control.Category
import           Control.Arrow hiding (loop)
import           Control.Arrow.Fix.Chaotic
import           Control.Arrow.Fix.Cache as Cache
import           Control.Arrow.Fix.ControlFlow
import           Control.Arrow.Fix.Stack as Stack
import           Control.Arrow.Fix.Context as Context
import           Control.Arrow.State
import           Control.Arrow.Trans
import           Control.Arrow.Order

-- import           Control.Arrow.Transformer.Writer
import           Control.Arrow.Transformer.State

import           Data.Profunctor
import           Data.Identifiable
import           Data.Coerce
import           Data.Empty
import           Data.Order hiding (lub)
import           Data.HashSet(HashSet)
import qualified Data.HashSet as H


-- newtype ComponentT a c x y = ComponentT (WriterT (Component a) c x y)
newtype ComponentT component a c x y = ComponentT (StateT (component a) c x y)
  deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowStack a,ArrowStackDepth,ArrowStackElements a,
            ArrowCache a b,ArrowParallelCache a b,ArrowIterateCache,
            ArrowContext ctx, ArrowJoinContext u, ArrowControlFlow stmt)

runComponentT :: (IsEmpty (comp a), Profunctor c) => ComponentT comp a c x y -> c x y
runComponentT (ComponentT f) = dimap (empty,) snd (runStateT f)
{-# INLINE runComponentT #-}

instance ArrowLift (ComponentT comp a) where
  lift' f = ComponentT (lift' f)
  {-# INLINE lift' #-}

instance (IsEmpty (comp a), ArrowRun c) => ArrowRun (ComponentT comp a c) where
  type Run (ComponentT comp a c) x y = Run c x y
  run f = run (runComponentT f)
  {-# INLINE run #-}

instance (Identifiable a, Profunctor c,ArrowApply c) => ArrowApply (ComponentT comp a c) where
  app = ComponentT (lmap (first coerce) app)
  {-# INLINE app #-}

instance ArrowState s c => ArrowState s (ComponentT comp a c) where
  get = lift' get
  put = lift' put
  {-# INLINE get #-}
  {-# INLINE put #-}

instance (Arrow c, Profunctor c) => ArrowJoin (ComponentT comp a c) where
  joinSecond lub f (ComponentT g) = ComponentT (rmap (\(x,y) -> f x `lub` y) (id &&& g))
  {-# INLINE joinSecond #-}

instance (Complete y, ArrowEffectCommutative c) => ArrowComplete y (ComponentT comp a c) where
  ComponentT f <⊔> ComponentT g = ComponentT $ rmap (uncurry (⊔)) (f &&& g)
  {-# INLINE (<⊔>) #-}

instance (Identifiable a, ArrowEffectCommutative c) => ArrowEffectCommutative (ComponentT comp a c)

-- Standard Component ----------------------------------------------------------------------------------
newtype Component a = Component (HashSet a) deriving (Eq,PreOrd,Complete,IsEmpty)

instance ArrowTrans (ComponentT Component a c) where
  type Underlying (ComponentT Component a c) x y = c (Component a,x) (Component a,y)

instance (Identifiable a, Arrow c, Profunctor c) => ArrowComponent a (ComponentT Component a c) where
  addToComponent = lift $ arr $ \(Component c,a) -> (Component (H.insert a c),())
  removeFromComponent = lift $ arr $ \(Component c,a) -> (Component (H.delete a c),())
  {-# INLINE addToComponent #-}
  {-# INLINE removeFromComponent #-}

instance (Identifiable a, Arrow c, Profunctor c) => ArrowInComponent a (ComponentT Component a c) where
  inComponent = lift $ arr $ \(Component c,a) ->
    let comp | H.null c = Empty
             | H.singleton a == c = Head Outermost
             | H.member a c = Head Inner
             | otherwise = Body
    in (Component c,comp)
  {-# INLINE inComponent #-}

-- Second Projection ----------------------------------------------------------------------------------
data Proj2 comp b where
  Proj2 :: comp b -> Proj2 comp (a,b)

instance IsEmpty (comp b) => IsEmpty (Proj2 comp (a,b)) where
  empty = Proj2 empty

instance (Arrow c, Profunctor c, ArrowComponent b (ComponentT comp b c)) => ArrowComponent (a,b) (ComponentT (Proj2 comp) (a,b) c) where
  addToComponent = lift (lmap snd addToComponent)
  removeFromComponent = lift (lmap snd removeFromComponent)
  {-# INLINE addToComponent #-}
  {-# INLINE removeFromComponent #-}

instance (Profunctor c, Arrow c, ArrowInComponent b (ComponentT comp b c)) => ArrowInComponent (a,b) (ComponentT (Proj2 comp) (a,b) c) where
  inComponent = lift (lmap snd inComponent)
  {-# INLINE inComponent #-}

instance Profunctor c => ArrowTrans (ComponentT (Proj2 comp) (a,b) c) where
  type Underlying (ComponentT (Proj2 comp) (a,b) c) x y = ComponentT comp b c x y
  lift (ComponentT f) = ComponentT $ lift $ dimap (first (\(Proj2 c) -> c)) (first Proj2) (unlift f)
  unlift (ComponentT f) = ComponentT $ lift $ dimap (first Proj2) (first (\(Proj2 c) -> c)) (unlift f)
  {-# INLINE lift #-}
  {-# INLINE unlift #-}
