{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Control.Arrow.Transformer.Abstract.Fix.Component(ComponentT,runComponentT,Component,Monotone) where

import           Prelude hiding (id,pred,lookup,map,head,iterate,(.),elem)

import           Control.Category
import           Control.Arrow hiding (loop)
import           Control.Arrow.Strict
import           Control.Arrow.Fix.Chaotic
import           Control.Arrow.Fix.Cache as Cache
import           Control.Arrow.Fix.ControlFlow
import           Control.Arrow.Fix.Stack as Stack
import           Control.Arrow.Fix.Context as Context
import           Control.Arrow.State
import           Control.Arrow.Trans

-- import           Control.Arrow.Transformer.Writer
import           Control.Arrow.Transformer.State

import           Data.Profunctor
import           Data.Identifiable
import           Data.Coerce
import           Data.Empty
import           Data.Order hiding (lub)
import           Data.HashSet(HashSet)
import qualified Data.HashSet as H
import           Data.HashMap.Strict(HashMap)
import qualified Data.HashMap.Strict as M

newtype ComponentT component a c x y = ComponentT (StateT (component a) c x y)
  deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowStrict,
            ArrowStack a,ArrowStackDepth,ArrowStackElements a,
            ArrowCache a b, ArrowParallelCache a b,ArrowIterateCache,
            ArrowContext ctx, ArrowJoinContext u, ArrowControlFlow stmt)

runComponentT :: (IsEmpty (comp a), Profunctor c) => ComponentT comp a c x y -> c x y
runComponentT (ComponentT f) = dimap (\x -> (empty,x)) snd (runStateT f)
{-# INLINE runComponentT #-}

instance ArrowTrans (ComponentT comp a c) where
  type Underlying (ComponentT comp a c) x y = c (comp a,x) (comp a,y)

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

-- Standard Component ----------------------------------------------------------------------------------
newtype Component a = Component (HashSet a) deriving (Eq,PreOrd,Complete,IsEmpty)

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

-- Component with a mononone part ------------------------------------------------------------------
data Monotone b where
  Monotone :: HashMap b a -> Monotone (a,b)

instance IsEmpty (Monotone (a,b)) where
  empty = Monotone empty

instance (PreOrd a, Identifiable b, Arrow c, Profunctor c) => ArrowComponent (a,b) (ComponentT Monotone (a,b) c) where
  addToComponent      = lift $ arr $ \(Monotone m,(a,b)) -> (Monotone (M.insert b a m), ())
  removeFromComponent = lift $ arr $ \(Monotone m,(a,b)) ->
    (Monotone (M.update (\a' -> let pred = a' ⊑ a in pred `seq` if pred then Nothing else Just a') b m), ())
  {-# INLINE addToComponent #-}
  {-# INLINE removeFromComponent #-}

instance (PreOrd a, Identifiable b, Profunctor c, Arrow c) => ArrowInComponent (a,b) (ComponentT Monotone (a,b) c) where
  inComponent = lift $ arr $ \(Monotone m,(a,b)) ->
    let comp | M.null m = Empty
             | Just a ⊑ M.lookup b m = Head $ if M.size m == 1 then Outermost else Inner
             | otherwise = Body
    in (Monotone m,comp)
  {-# INLINE inComponent #-}

