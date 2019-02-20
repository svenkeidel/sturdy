{-# LANGUAGE Arrows #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Transformer.Abstract.Fix(FixT,runFixT,runFixT') where

import           Prelude hiding (id,(.),const,head,iterate)

import           Control.Category
import           Control.Arrow hiding (loop)
import           Control.Arrow.Deduplicate
import           Control.Arrow.Const
import           Control.Arrow.Fix
import           Control.Arrow.Reader
import           Control.Arrow.State
import           Control.Arrow.Trans
import           Control.Arrow.Abstract.Join
import           Control.Arrow.Transformer.Const
import           Control.Arrow.Transformer.Static
import           Control.Arrow.Transformer.Reader
import           Control.Arrow.Transformer.State
import           Control.Arrow.Utils (const,(&&>))

import           Data.Identifiable
import           Data.Order
import           Data.Abstract.Widening (Widening,Stable(..))
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
type Print a = a -> String
data Constant s a b = Constant
  { printDom :: Print (a,a)
  , printCod :: Print b
  , stackWidening :: StackWidening s a
  , widening :: Widening b
  }
newtype FixT s a b c x y = FixT { unFixT :: ConstT (Constant s a b) (ReaderT (s a) (StateT (Cache a b, Component a) c)) x y }
  deriving (Profunctor,Category,Arrow,ArrowChoice)
type instance Fix x y (FixT s () () c) = FixT s x y c

runFixT :: (Identifiable a, PreOrd b, Monoid (s a),ArrowChoice c, Profunctor c) => StackWidening s a -> Widening b -> FixT s a b c x y -> c x y
runFixT = runFixT' (error "use runFixT'") (error "use runFixT'")

runFixT' :: (Identifiable a, PreOrd b, Monoid (s a),ArrowChoice c, Profunctor c) => Print (a,a) -> Print b -> StackWidening s a -> Widening b -> FixT s a b c x y -> c x y
runFixT' pa pb sw w f = dimap (\x -> ((M.empty,H.empty),(mempty,x))) snd $ runStateT $ runReaderT $ runConstT (Constant pa pb sw w) $ unFixT f

#ifndef TRACE
instance (Identifiable a, LowerBounded b, Profunctor c,ArrowChoice c) => ArrowFix a b (FixT s a b c) where
  fix f = FixT $ stackWiden'
      (let iterate = proc (x,x') -> do
             -- If we are not in a loop, continue recursing.
             (y,(member,head)) <- unFixT (f (fix f)) &&& inComponent -< x'
             if member
               then do
                 (stable,yNew) <- updateCache -< (x',y)
                 
                 -- If we did not reach a fixpoint of f(x'), keep iterating.
                 if head && not stable
                   then iterate -< (x,x')
                   else returnA -< yNew
               else returnA -< y
       in iterate)

      -- If we are in a loop, return the cached value or bottom otherwise.
      -- Furthermore, add x' to the current component.
      (lmap snd $ addToComponent &&> initializeCache)

#else

instance (Identifiable a, LowerBounded b, Profunctor c,ArrowChoice c) => ArrowFix a b (FixT s a b c) where
  fix f = FixT $ stackWiden'
      (let iterate = proc (x,x') -> do
             c <- askConst -< ()
             -- If we are not in a loop, continue recursing.
             (y,(member,head)) <- unFixT (f (fix f)) &&& inComponent -< trace (printf "call    [%s]" (printDom c (x,x'))) x'
             if member
               then do
                 (stable,yNew) <- updateCache -< (x',y)
                 
                 -- If we did not reach a fixpoint of f(x'), keep iterating.
                 if head && not stable
                   then iterate -< trace (printf "iterate [%s] = %s" (printDom c (x,x')) (printCod c yNew)) (x,x')
                   else returnA -< if head then trace (printf "fixed   [%s] = %s" (printDom c (x,x')) (printCod c yNew)) yNew else yNew
               else returnA -< trace (printf "return  [%s] = %s" (printDom c (x,x')) (printCod c y)) y
       in iterate)

      -- If we are in a loop, return the cached value or bottom otherwise.
      -- Furthermore, add x' to the current component.
      (proc (x,x') -> do
         c <- askConst -< ()
         y <- addToComponent &&> initializeCache -< x'
         returnA -< trace (printf "loop    [%s] = %s" (printDom c (x,x')) (printCod c y)) y)
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

instance (Arrow c, Profunctor c, LowerBounded (c x y)) => LowerBounded (FixT s a b c x y) where
  bottom = lift' bottom

instance ArrowLift (FixT s a b) where
  lift' = FixT . lift' . lift' . lift'

instance (ArrowApply c, Profunctor c) => ArrowApply (FixT s a b c) where
  app = FixT (lmap (first unFixT) app)

instance (Arrow c, Profunctor c) => ArrowDeduplicate x y (FixT s a b c) where
  dedup f = f

----- Helper functions -----
stackWiden' :: (ArrowReader (s a) c,ArrowChoice c) => ConstT (Constant s a b) c (a,a) b -> ConstT (Constant s a b) c (a,a) b -> ConstT (Constant s a b) c a b
stackWiden' (ConstT (StaticT f)) (ConstT (StaticT g)) =
  constT $ \c -> 
       rmap (\(s,x) -> let ~(s',(l,x')) = stackWidening c s x
                       in case l of
                           NoLoop -> Left (s',(x,x'))
                           Loop   -> Right (x,x')
            ) (const ask &&& id)
       >>>
       (local (f c) ||| g c)

initializeCache :: (Identifiable a, LowerBounded b, ArrowState (Cache a b,Component a) c) => c a b
initializeCache = modifyCache (\x -> insertWithLookup (\_ old -> old) x bottom)

updateCache :: (Identifiable a, LowerBounded b, ArrowState (Cache a b,Component a) c) => ConstT (Constant s a b) c (a,b) (Bool,b)
updateCache = constT $ \c -> modifyCache $ \(x,y) cache -> case M.lookup x cache of
  Just yOld -> let (s,yNew) = widening c yOld y in ((s == Stable,yNew),M.insert x yNew cache)
  Nothing   -> ((False,y),M.insert x y cache)

inComponent :: (Identifiable a, Arrow c, Profunctor c, ArrowState (Cache a b,Component a) c) => c a (Bool,Bool)
inComponent = modifyComp $ \x comp -> ((not (H.null comp) && H.member x comp, H.singleton x == comp), H.delete x comp)

addToComponent :: (Identifiable a,ArrowState (Cache a b,Component a) c) => c a ()
addToComponent = modifyComp (\x comp -> ((),H.insert x comp))

modifyCache :: ArrowState (Cache a b,Component a) c => (x -> Cache a b -> (y,Cache a b)) -> c x y
modifyCache f = modify (arr $ \(x,(cache,comp)) -> let (y,cache') = f x cache in (y,(cache',comp)))

modifyComp :: ArrowState (Cache a b,Component a) c => (x -> Component a -> (y,Component a)) -> c x y
modifyComp f = modify (arr $ \(x,(cache,comp)) -> let (y,comp') = f x comp in (y,(cache,comp')))

insertWithLookup :: Identifiable a => (b -> b -> b) -> a -> b -> HashMap a b -> (b,HashMap a b)
insertWithLookup w a b m =
  let m' = M.insertWith w a b m
  in (m' M.! a, m')

