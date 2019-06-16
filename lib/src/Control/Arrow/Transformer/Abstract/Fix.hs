{-# LANGUAGE Arrows #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
module Control.Arrow.Transformer.Abstract.Fix(FixT,runFixT) where

import           Prelude hiding (id,(.),const,head,iterate,lookup)

import           Control.Category
import           Control.Arrow hiding (loop)
import           Control.Arrow.Const
import           Control.Arrow.Deduplicate
import           Control.Arrow.Fix
import           Control.Arrow.Reader
import           Control.Arrow.State
import           Control.Arrow.Trans
import           Control.Arrow.Abstract.Join
import           Control.Arrow.Transformer.Const
import           Control.Arrow.Transformer.Static
import           Control.Arrow.Transformer.Reader
import           Control.Arrow.Transformer.State

import           Data.Identifiable
import           Data.Order
import           Data.HashSet (HashSet)
import qualified Data.HashSet as H
import           Data.Profunctor
import           Data.Coerce

import           Data.Abstract.Cache (IsCache,CachingStrategy,RecomputeOrCached(..))
import qualified Data.Abstract.Cache as Cache
import           Data.Abstract.StackWidening (StackWidening,Loop(..))
import           Data.Abstract.Widening (Stable(..),Widening)

type Component a = HashSet a
newtype FixT stack cache a b c x y = FixT { unFixT ::
  ConstT (StackWidening stack a,CachingStrategy cache a b,Widening b)
    (ReaderT (stack a) (StateT (Component a,cache a b) c)) x y }
  deriving (Profunctor,Category,Arrow,ArrowChoice)


runFixT :: (Identifiable a,PreOrd b,Monoid (stack a),IsCache cache a b,ArrowChoice c, Profunctor c)
  => StackWidening stack a -> CachingStrategy cache a b -> Widening b -> FixT stack cache a b c x y -> c x y
runFixT stackWiden cachingStrat widen (FixT f) =
  dimap (\x -> ((H.empty,Cache.empty),(mempty,x))) snd $
    runStateT $ runReaderT $ runConstT (stackWiden,cachingStrat,widen) f

type instance Fix x y (FixT stack cache () () c) = FixT stack cache x y c
instance (Identifiable a, LowerBounded b, Profunctor c,IsCache cache a b,ArrowChoice c,ArrowApply c) => ArrowFix a b (FixT stack cache a b c) where
  fix f = cached $ stackWidening $ proc (loop,x) -> do
    -- Apply a stack widening operator to the input value.
    -- The stack widening operator ensures that the abstract
    -- interpreter does not recurse infinitely deep and detects
    -- signals if the interpreter is in a loop. See
    -- `Data.Abstract.StackWidening` for details.
                             
    case loop of
      NoLoop ->
        -- If there is no loop, keep recursing.
        f (fix f) -< x

      MaybeLoop ->
        -- If we may be in a loop, continue recursing and check
        -- afterwards if the cached value has stabilized, otherwise keep
        -- iterating.
        let iterate = proc () -> do
               y <- f (fix f) -< x
               member <- inComponent -< x
               deleteFromComponent -< x
               if member
                 then do
                   -- Updates the cached value by applying a widening
                   -- operator on the old and the new `y` value. The
                   -- widening operator also signals if the cached
                   -- value stabilized, i.e., did not grow.
                   (stable,yNew) <- update -< (x,y)
                   
                   -- If we did not reach a fixpoint of f(x'), keep iterating.
                   case stable of
                     Instable -> iterate -< ()
                     Stable   -> returnA -< yNew

                 else returnA -< y

         in iterate -<< ()

      Loop -> do
         -- If we are in a loop, return the cached value or bottom otherwise.
         -- Furthermore, add x' to the current component.
         addToComponent -< x
         lookupInit -< x

instance (Identifiable a, ArrowJoin c, ArrowChoice c) => ArrowJoin (FixT stack cache a b c) where
  -- | The join operation of the 'FixT' arrow *does not* join the
  -- cache for efficiency.  This assumes that the cache is extended
  -- only monotonically.  Furthermore, this join operation *is not*
  -- commutative, but it is still associative and computes an upper
  -- bound.
  joinWith lub_ f g = rmap (uncurry lub_) (f &&& g)

instance (ArrowJoin (FixT stack cache a b c), Profunctor c, Complete y, PreOrd (c x y)) => Complete (FixT stack cache a b c x y) where
  f ⊔ g = joinWith (⊔) f g

instance PreOrd (c x y) => PreOrd (FixT stack cache a b c x y) where
  (⊑) = error "f ⊑ g  iff  forall x. snd (f x) ⊑ snd (g x)"

instance (Arrow c, Profunctor c, LowerBounded (c x y)) => LowerBounded (FixT stack cache a b c x y) where
  bottom = lift' bottom

instance ArrowLift (FixT stack cache a b) where
  lift' = FixT . lift' . lift' . lift'

instance (ArrowApply c, Profunctor c) => ArrowApply (FixT stack cache a b c) where
  app = FixT (lmap (first coerce) app)

instance (Arrow c, Profunctor c) => ArrowDeduplicate x y (FixT stack cache a b c) where
  dedup f = f

----- Helper functions -----
cached :: (ArrowChoice c,Profunctor c) => FixT stack cache a b c a b -> FixT stack cache a b c a b
cached (FixT f) = FixT $ askConst $ \(_,cachingStrat,_) -> proc a -> do
  r <- modify' (\(a,(comp,cache)) -> let (r,cache') = cachingStrat a cache in (r,(comp,cache'))) -< a
  case r of
    Recompute a' -> f -< a'
    Cached b -> returnA -< b

stackWidening :: (Arrow c,Profunctor c) => FixT stack cache a b c (Loop,a) b -> FixT stack cache a b c a b
stackWidening (FixT f) = FixT $ askConst $ \(stackWiden,_,_) -> proc x -> do
  stack <- ask -< ()
  let (stack',(loop,x')) = stackWiden stack x
  local f -< (stack',(loop,x'))

update :: (Identifiable a, IsCache cache a b, Arrow c, Profunctor c) => FixT stack cache a b c (a,b) (Stable,b)
update = FixT $ askConst $ \(_,_,widening) -> modify' $ \((a,b),(comp,cache)) -> let ((st,b'),cache') = Cache.update widening a b cache in ((st,b'),(comp,cache'))

lookupInit :: (Identifiable a, IsCache cache a b, Arrow c, Profunctor c) => FixT stack cache a b c a b
lookupInit = FixT $ modify' $ \(a,(comp,cache)) -> let (b,cache') = Cache.lookupInit a cache in (b,(comp,cache'))

inComponent :: (Identifiable a, Arrow c, Profunctor c) => FixT stack cache a b c a Bool
inComponent = FixT $ proc x -> do
  (comp,_) <- get -< ()
  returnA -< not (H.null comp) && H.member x comp

deleteFromComponent :: (Identifiable a, Arrow c, Profunctor c) => FixT stack chache a b c a ()
deleteFromComponent = FixT $ modify' $ \(x,(comp,cache)) -> ((),(H.delete x comp,cache))

addToComponent :: (Identifiable a, Arrow c, Profunctor c) => FixT stack cache a b c a ()
addToComponent = FixT $ modify' $ \(x,(comp,cache)) -> ((),(H.insert x comp,cache))
