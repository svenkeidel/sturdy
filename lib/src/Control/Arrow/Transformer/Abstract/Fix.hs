{-# LANGUAGE Arrows #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Transformer.Abstract.Fix(FixT,runFixT) where

import           Control.Arrow hiding (loop)
import           Control.Arrow.Const
import           Control.Arrow.Fix
import           Control.Arrow.Reader
import           Control.Arrow.State
import           Control.Arrow.Abstract.Join
import           Control.Arrow.Transformer.Const
import           Control.Arrow.Transformer.Reader
import           Control.Arrow.Transformer.State
import           Control.Category

import qualified Control.Monad.State as S

import           Data.Identifiable
import           Data.Order
import           Data.Abstract.Widening (Widening)
import           Data.Abstract.StackWidening (StackWidening,Loop(..))
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.HashSet (HashSet)
import qualified Data.HashSet as H
import           Data.Profunctor

#ifdef TRACE
import           Debug.Trace
import           Text.Printf
#endif

type Cache a b = HashMap a b
type Component a = HashSet a
newtype FixT s a b c x y = FixT { unFixT :: ConstT (StackWidening s a, Widening b) (ReaderT (s a) (StateT (Cache a b, Component a) c)) x y }
  deriving (Profunctor,Category,Arrow,ArrowChoice)
type instance Fix x y (FixT s () () c) = FixT s x y c

runFixT :: (Identifiable a, PreOrd b, Monoid (s a),ArrowChoice c, Profunctor c) => StackWidening s a -> Widening b -> FixT s a b c x y -> c x y
runFixT sw w f = dimap (\x -> ((M.empty,H.empty),(mempty,x))) snd $ runStateT $ runReaderT $ runConstT (sw,w) $ unFixT f

#ifndef TRACE
instance (Identifiable a, LowerBounded b, Profunctor c,ArrowChoice c) => ArrowFix a b (FixT s a b c) where
  fix f = FixT $ proc x -> do
    (stackWiden,stack) <- getStackWidening -< ()
    let ((loop,x'),stack') = S.runState (stackWiden x) stack
    case loop of
      -- If we are not in a loop, continue recursing.
      NoLoop -> do
        y <- local (unFixT (f (fix f))) -< (stack',x')

        comp <- getComponent -< ()
        if not (H.null comp) && x' `H.member` comp
          then do
            yOld <- updateCache -< (x',bottom)
            yNew <- updateCache -< (x',y)
            deleteFromComponent -< x'
            
            -- If we did not reach a fixpoint of f(x'), keep iterating.
            if x' `isHeadOf` comp && yOld ⊏ yNew
              then unFixT (fix f) -< x
              else returnA -< yNew

          else do
            returnA -< y

      -- If we are in a loop, return the cached value or bottom otherwise.
      -- Furthermore, add x' to the current component.
      Loop -> do
        addToComponent -< x'
        updateCache -< (x',bottom)
    where
      y ⊏ y' = {- y ⊑ y' && -} not (y' ⊑ y)

#else

instance (Show a, Show b, Show (s a), Identifiable a, LowerBounded b, Profunctor c,ArrowChoice c) => ArrowFix a b (FixT s a b c) where
  fix f = FixT $ proc x -> do
    (stackWiden,stack) <- getStackWidening -< ()
    let ((loop,x'),stack') = S.runState (stackWiden x) stack
    case loop of
      NoLoop -> do
        y <- local (unFixT (f (fix f))) -< trace (printf "call    [%s -> %s, %s]" (show x) (show x') (show stack)) $ (stack',x')

        comp <- getComponent -< ()
        if not (H.null comp) && x' `H.member` comp
          then do
            yOld <- updateCache -< (x',bottom)
            yNew <- updateCache -< (x',y)
            deleteFromComponent -< x'
            
            if x' `isHeadOf` comp && yOld ⊏ yNew
              then unFixT (fix f) -< trace (printf "iterate [%s -> %s, %s] = [%s -> %s]" (show x) (show x') (show stack) (show yOld) (show yNew)) $ x
              else returnA -< if x' `isHeadOf` comp then trace (printf "fixed   [%s -> %s] = %s" (show x) (show x') (show yNew)) yNew else yNew

          else do
            returnA -< y 
            -- returnA -< trace (printf "return  [%s -> %s] = %s" (show x) (show x') (show y)) y 

      -- If we are in a loop, return the cached value or bottom otherwise.
      -- Furthermore, add x' to the current component.
      Loop -> do
        addToComponent -< x'
        y <- updateCache -< (x',bottom)
        -- returnA -< y
        returnA -< trace (printf "loop    [%s -> %s, %s] = %s" (show x) (show x') (show stack) (show y)) y
        
    where
      y ⊏ y' = {- y ⊑ y' && -} not (y' ⊑ y)

#endif

instance (ArrowJoin c, ArrowChoice c) => ArrowJoin (FixT s a b c) where
  -- | The join operation of the 'FixT' arrow *does not* join the
  -- cache for efficiency.  This assumes that the cache is extended
  -- only monotonically.  Furthermore, this join operation *is not*
  -- commutative, but it is still associative and computes an upper
  -- bound.
  joinWith lub_ f g = rmap (uncurry lub_) (f &&& g)

instance (ArrowJoin (FixT s a b c), Profunctor c, Complete y, PreOrd (c x y)) => Complete (FixT s a b c x y) where
  f ⊔ g = joinWith (⊔) f g

instance PreOrd (c x y) => PreOrd (FixT s a b c x y) where
  (⊑) = error "f ⊑ g  iff  forall x. snd (f x) ⊑ snd (g x)"

instance (ArrowApply c, Profunctor c) => ArrowApply (FixT s a b c) where
  app = FixT (lmap (first unFixT) app)

----- Helper functions -----

getStackWidening :: (ArrowConst (StackWidening s a,r) c, ArrowReader (s a) c) => c () (StackWidening s a,s a)
getStackWidening = rmap (first fst) (askConst &&& ask)

updateCache :: (Identifiable a, LowerBounded b, ArrowState (Cache a b,Component a) c) => ConstT (r,Widening b) c (a,b) b
updateCache = constT $ \(_,widen) -> modify' (\(x,y) -> insertWithLookup (\new old -> if new ≈ bottom then old else widen old new) x y) (curry snd)

getComponent :: ArrowState (Cache a b,Component a) c => c () (Component a)
getComponent = rmap snd get

addToComponent :: (Identifiable a,ArrowState (Cache a b,Component a) c) => c a ()
addToComponent = modify (arr (\(x,(cache,comp)) -> ((),(cache,H.insert x comp))))

isHeadOf :: Identifiable a => a -> Component a -> Bool
isHeadOf x comp = H.singleton x == comp

deleteFromComponent :: (Identifiable a,ArrowState (Cache a b,Component a) c) => c a ()
deleteFromComponent = modify (arr (\(x,(cache,comp)) -> ((),(cache,H.delete x comp))))

modify' :: ArrowState (Cache a b,Component a) c => (x -> Cache a b -> (y,Cache a b)) -> (x -> Component a -> Component a) -> c x y
modify' f g = modify (arr $ \(x,(cache,comp)) -> let (y,cache') = f x cache in (y,(cache',g x comp)))

insertWithLookup :: Identifiable a => (b -> b -> b) -> a -> b -> HashMap a b -> (b,HashMap a b)
insertWithLookup w a b m =
  let m' = M.insertWith w a b m
  in (m' M.! a, m')

