{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Control.Arrow.Transformer.Abstract.Fix.Component(ComponentT,runComponentT,Component) where

import           Prelude hiding (id,pred,lookup,map,head,iterate,(.),elem)

import           Control.Category
import           Control.Arrow hiding (loop)
import           Control.Arrow.Primitive
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

import           Data.Bits
import           Data.Profunctor
import           Data.Identifiable
import           Data.Coerce
import           Data.Empty

import           Text.Printf

newtype ComponentT component a c x y = ComponentT (StateT (component a) c x y)
  deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowStrict,
            ArrowStackDepth,ArrowStackElements a,
            ArrowCache a b, ArrowParallelCache a b,ArrowIterateCache a b,ArrowGetCache cache,
            ArrowContext ctx, ArrowJoinContext u, ArrowControlFlow stmt, ArrowPrimitive)

runComponentT :: (IsEmpty (comp a), Profunctor c) => ComponentT comp a c x y -> c x y
runComponentT (ComponentT f) = dimap (\x -> (empty,x)) snd (runStateT f)
{-# INLINE runComponentT #-}

instance ArrowLift (ComponentT comp a c) where
  type Underlying (ComponentT comp a c) x y = c (comp a,x) (comp a,y)

instance ArrowTrans (ComponentT comp a) where
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

newtype Component a = Component Integer

instance Show (Component a) where
  show (Component comp) = printf "%b" comp

instance IsEmpty (Component a) where
  empty = mempty

instance Semigroup (Component a) where
  Component c1 <> Component c2 = Component (c1 .|. c2)
instance Monoid (Component a) where
  mempty = Component 0
  mappend = (<>)

instance (Arrow c, Profunctor c) => ArrowComponent a (ComponentT Component a c) where
  addToComponent = lift $ arr $ \(Component comp,(_,pointer)) ->
    let comp' = (shiftL (1 :: Integer) pointer .|. comp)
    in (Component comp', ())
  {-# INLINE addToComponent #-}
  {-# SCC addToComponent #-}

instance (Identifiable a, ArrowStack a c) => ArrowStack a (ComponentT Component a c) where
  push f = lift $ proc (comp,(a,x)) -> do
    (comp',y) <- push (lmap (\x -> (mempty,x)) (unlift f)) -< (a,x)
    returnA -< (comp <> pop comp', y)
    where
      pop (Component comp) = Component (shiftR comp 1)
  {-# INLINE push #-}
  {-# SCC push #-}

instance (Arrow c, Profunctor c) => ArrowInComponent a (ComponentT Component a c) where
  inComponent f = lift $ dimap (second snd) (\(comp, y) -> (comp,(isInComponent comp,y))) (unlift f)
    where
      isInComponent (Component comp)
        | comp == 0      = Empty
        | comp == 1      = Head Outermost
        | testBit comp 0 = Head Inner
        | otherwise      = Body
  {-# INLINE inComponent #-}
  {-# SCC inComponent #-}

-- Standard Component ----------------------------------------------------------------------------------
-- newtype Component a = Component (HashSet a) deriving (Eq,IsEmpty,Monoid,Semigroup)

-- instance (Identifiable a, Arrow c, Profunctor c) => ArrowComponent a (ComponentT Component a c) where
--   addToComponent = lift $ arr $ \(Component c,a) -> (Component (H.insert a c),())
--   removeFromComponent = lift $ arr $ \(Component c,a) -> (Component (H.delete a c),())
--   {-# INLINE addToComponent #-}
--   {-# INLINE removeFromComponent #-}
--   {-# SCC addToComponent #-}
--   {-# SCC removeFromComponent #-}

-- instance (Identifiable a, Arrow c, Profunctor c) => ArrowInComponent a (ComponentT Component a c) where
--   inComponent = lift $ arr $ \(Component c,a) ->
--     let comp | H.null c = Empty
--              | H.singleton a == c = Head Outermost
--              | H.member a c = Head Inner
--              | otherwise = Body
--     in (Component c,comp)
--   {-# INLINE inComponent #-}
--   {-# SCC inComponent #-}

-- instance (Identifiable a, ArrowStack a c) => ArrowStack a (ComponentT Component a c) where
--   push f = lift $ proc (comp,(a,x)) -> do
--     (comp',y) <- push (lmap (\x -> (mempty,x)) (unlift f)) -< (a,x)
--     returnA -< (comp <> comp', y)
--   {-# INLINE push #-}
--   {-# SCC push #-}

-- -- Component with a mononone part ------------------------------------------------------------------
-- data Monotone b where
--   Monotone :: HashMap b a -> Monotone (a,b)

-- instance IsEmpty (Monotone (a,b)) where
--   empty = Monotone empty

-- instance Identifiable b => Semigroup (Monotone (a,b)) where
--   Monotone m1 <> Monotone m2 = Monotone (M.union m2 m1)

-- instance Identifiable b => Monoid (Monotone (a,b)) where
--   mempty = empty
--   mappend = (<>)

-- instance (PreOrd a, Identifiable b, Arrow c, Profunctor c) => ArrowComponent (a,b) (ComponentT Monotone (a,b) c) where
--   addToComponent      = lift $ arr $ \(Monotone m,(a,b)) -> (Monotone (M.insert b a m), ())
--   removeFromComponent = lift $ arr $ \(Monotone m,(a,b)) ->
--     (Monotone (M.update (\a' -> if a' ⊑ a then Nothing else Just a') b m), ())
--   {-# INLINE addToComponent #-}
--   {-# INLINE removeFromComponent #-}
--   {-# SCC addToComponent #-}
--   {-# SCC removeFromComponent #-}

-- instance (PreOrd a, Identifiable b, Profunctor c, Arrow c) => ArrowInComponent (a,b) (ComponentT Monotone (a,b) c) where
--   inComponent = lift $ arr $ \(Monotone m,(a,b)) ->
--     let comp | M.null m              = Empty
--              | Just a ⊑ M.lookup b m = Head $ if M.size m == 1 then Outermost else Inner
--              | otherwise             = Body
--     in (Monotone m,comp)
--   {-# INLINE inComponent #-}
--   {-# SCC inComponent #-}

-- instance (ArrowStack a c) => ArrowStack a (ComponentT Monotone a c) where
--   push f = lift $ proc (comp,(a,x)) -> push (unlift f) -< (a,(comp,x))
--   {-# INLINE push #-}
--   {-# SCC push #-}
