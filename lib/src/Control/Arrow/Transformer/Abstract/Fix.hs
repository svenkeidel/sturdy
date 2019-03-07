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
module Control.Arrow.Transformer.Abstract.Fix(FixT,runFixT,runFixT') where

import           Prelude hiding (id,(.),const,head,iterate)

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
import           Data.Abstract.Widening (Widening,Stable(..))
import           Data.Abstract.StackWidening (StackWidening,Loop(..))
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.HashSet (HashSet)
import qualified Data.HashSet as H
import           Data.Profunctor

newtype FixT s a b c x y = FixT { unFixT :: ConstT (Constant s a b) (ReaderT (s a) (StateT (Cache a b, Component a) c)) x y }
  deriving (Profunctor,Category,Arrow,ArrowChoice)

type Cache a b = HashMap a (Stable,b)
type Component a = HashSet a
type Print a = a -> String
data Constant s a b = Constant
  { printDom :: Print (a,a)
  , printCod :: Print b
  , stackWidening :: StackWidening s a
  , widening :: Widening b
  }

runFixT :: (Identifiable a, PreOrd b, Monoid (s a),ArrowChoice c, Profunctor c) => StackWidening s a -> Widening b -> FixT s a b c x y -> c x y
runFixT = runFixT' (error "use runFixT'") (error "use runFixT'")

runFixT' :: (Identifiable a, PreOrd b, Monoid (s a),ArrowChoice c, Profunctor c) => Print (a,a) -> Print b -> StackWidening s a -> Widening b -> FixT s a b c x y -> c x y
runFixT' pa pb sw w f = dimap (\x -> ((M.empty,H.empty),(mempty,x))) snd $ runStateT $ runReaderT $ runConstT (Constant pa pb sw w) $ unFixT f

type instance Fix x y (FixT s () () c) = FixT s x y c
instance (Identifiable a, LowerBounded b, Profunctor c,ArrowChoice c,ArrowApply c) => ArrowFix a b (FixT s a b c) where
  fix f = FixT $ proc x -> do
    -- Apply a stack widening operator to the input value.
    -- The stack widening operator ensures that the abstract
    -- interpreter does not recurse infinitely deep and detects
    -- signals if the interpreter is in a loop. See
    -- `Data.Abstract.StackWidening` for details.
    c <- askConst -< ()
    stack <- ask -< ()
    let (stack',(loop,x')) = stackWidening c stack x
                             
    case loop of
      NoLoop ->
        -- If there is no loop, keep recursing.
        local (unFixT (f (fix f))) -< (stack',x')

      MaybeLoop ->
        -- If we may be in a loop, continue recursing and check
        -- afterwards if the cached value has stabilized, otherwise keep
        -- iterating.
        let iterate = proc () -> do
               y <- local (unFixT (f (fix f))) -< (stack',x')
               member <- inComponent -< x'
               if member
                 then do
                   -- Updates the cached value by applying a widening
                   -- operator on the old and the new `y` value. The
                   -- widening operator also signals if the cached
                   -- value stabilized, i.e., did not grow.
                   (stable,yNew) <- updateCache -< (x',y)
                   
                   -- If we did not reach a fixpoint of f(x'), keep iterating.
                   case stable of
                     Instable -> iterate -< ()
                     Stable   -> returnA -< yNew

                 else returnA -< y

         in local iterate -<< (stack',())

      Loop -> do
         -- If we are in a loop, return the cached value or bottom otherwise.
         -- Furthermore, add x' to the current component.
         addToComponent -< x'
         initializeCache -< x'

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

initializeCache :: (Identifiable a, LowerBounded b, ArrowState (Cache a b,Component a) c) => c a b
initializeCache = modifyCache (\x -> first snd . insertWithLookup (\_ old -> old) x (Instable,bottom))

updateCache :: (Identifiable a, LowerBounded b, ArrowState (Cache a b,Component a) c) => ConstT (Constant s a b) c (a,b) (Stable,b)
updateCache = constT $ \c -> modifyCache $ \(x,y) cache -> case M.lookup x cache of
  Just (_,yOld) -> let yNew = widening c yOld y in (yNew,M.insert x yNew cache)
  Nothing   -> ((Instable,y),M.insert x (Instable,y) cache)

inComponent :: (Identifiable a, Arrow c, Profunctor c, ArrowState (Cache a b,Component a) c) => c a (Bool)
inComponent = modifyComp $ \x comp -> ((not (H.null comp) && H.member x comp), H.delete x comp)

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

