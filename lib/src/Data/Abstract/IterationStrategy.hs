{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Data.Abstract.IterationStrategy where

import           Prelude hiding (pred,lookup,map)

import           Control.Arrow(second)

import           Data.Order
import           Data.HashMap.Lazy(HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.HashSet(HashSet)
import qualified Data.HashSet as H
import           Data.Identifiable
import           Data.Lens (Prism',getMaybe,set)
import           Data.Empty

import           Data.Abstract.StackWidening(StackWidening,Stack(..),type (**)(..))
import qualified Data.Abstract.StackWidening as Stack
import           Data.Abstract.Widening(Widening,Stable(..))

import           Text.Printf
import qualified Debug.Trace as Debug

-- | The iteration strategy @▽@ ensures that a recursive computation
-- terminates, soundly approximates the fixed point and avoids
-- redundant computation.
--
-- It ensures termination by transforming an infinite series
-- @x1, x2, x3 ...@, to a finite series:
-- @
--   x1 ▽ s1 = (Compute x1',s2)  &  x1 ⊑ x1'
--   x2 ▽ s2 = (Compute x2',s3)  &  x2 ⊑ x2'
--   x3 ▽ s3 = (Compute x3',s4)  &  x3 ⊑ x3'
--   ...
--   xn ▽ sn = (Cached y,sn+1)
-- @
-- Of course this is only sound if we iterate on inputs that
-- recursively depend on themselves. For example,
-- @
--   x1 ▽ s1 = (ComputeAndIterate x1',s2)
--   x2 ▽ s2 = (Compute x2',s3)
--   x3 ▽ s3 = (Cached (lookupDefault x1' bottom s3),s4)
-- @
-- Because @x3 ⊑ x1'@, we can reuse the result of @x1'@ at the loss of
-- precision. However, since the result of @x1'@ has not been computed
-- yet, we initialize the result with @bottom@ and iterate on it until
-- it stabilized.
--
-- The iteration strategy can also avoid redundant computation by
-- returning cached results instead of recomputing them.

type IterationStrategy stack cache a b = a -> (stack a,cache a b) -> (Compute cache a b,(stack a,cache a b))
data Compute cache a b = Compute a | ComputeAndIterate a (Update cache a b) | Cached a b
type Update cache a b = b -> cache a b -> (Iterate b, cache a b)
data Iterate b = Iterate | Return b

finite :: IterationStrategy Stack.Unit Unit a b
finite a sc = (Compute a,sc)

trace :: (Show a, Show b, Show (stack a), Show (cache a b))
      => IterationStrategy stack cache a b -> IterationStrategy stack cache a b
trace strat a (s,c) =
  let (r,(s',c')) = strat a (s,c)
  in Debug.trace (printf "x: %s\nstrat: %s\nstack: %s\ncache: %s\nstack: %s\ncache': %s\n\n"
                 (show a) (show r) (show s) (show c) (show s') (show c')) (r,(s',c'))

filter :: Prism' a a' -> IterationStrategy stack cache a' b -> IterationStrategy (Stack.Const stack a') (Const cache a' b) a b
filter pred strat a sc@(Stack.Const s, Const c) = case getMaybe pred a of
  Just a' ->
    let (r,(s',c')) = strat a' (s,c)
    in (map' (\x -> set pred x a) Const getConst r,(Stack.Const s',Const c'))
  Nothing -> (Compute a,sc)

chaotic :: (Show a, Show b, Identifiable a,LowerBounded b) => StackWidening s a -> Widening b -> IterationStrategy (Stack ** s) Cache a b
chaotic stackWiden widen a0 (Product (Stack xs) stack,c) = case M.lookup a (store c) of
  Just (b,Stable) -> (Cached a b,(s',c))
  Just (b,Instable) | H.member a xs -> let c' = c { componentHead = H.insert a (componentHead c) } in (Cached a b,(s',c'))
  _ -> let c' = c { store = M.insertWith (\_ old -> old) a (bottom,Instable) (store c) } in (ComputeAndIterate a update,(s',c'))
  where
    (a,stack') = stackWiden a0 stack
    s' = Product (Stack (H.insert a xs)) stack'
  
    update b cache
      -- The call did not depend on any unstable calls. This means we are done and don't need to iterate.
      | H.null (componentHead cache) =
          let cache' = cache { store = M.insert a (b,Stable) (store cache)}
          in -- Debug.trace (printf "update stable\nx: %s\nyNew: %s\nstack: %s\ncache: %s\ncache': %s\n\n" (show a) (show b) (show (H.toList xs)) (show cache) (show cache'))
             (Return b,cache')

      -- We are at the head of a fixpoint component. This means, we have to iterate until the head stabilized.
      | H.singleton a == componentHead cache =
        let (bOld,_) = M.lookupDefault (bottom,Instable) a (store cache)
            (stable,bNew) = widen bOld b
            cache' = cache { componentHead = H.empty, componentBody = H.empty
                           , store = M.insert a (bNew,stable) (store cache)
                           }
        
        in case stable of
             -- If the head of a fixpoint component is stable, flag all elements in the body of the component as stable too and return.
             Stable   ->
               let cache'' = cache' { store = foldl (\st a' -> M.adjust (second (\_ -> Stable)) a' st) (store cache') (componentBody cache)}
               in -- Debug.trace (printf "update head stable\nx: %s\nyOld: %s\nyNew: %s\nyWiden: %s\nstack: %s\ncache: %s\ncache': %s\n\n" (show a) (show bOld) (show b) (show bNew) (show (H.toList xs)) (show cache) (show cache''))
                  (Return bNew,cache'')

             -- If the head of a fixpoint component is not stable, keep iterating.
             Instable ->
               -- Debug.trace (printf "update head iterate\nx: %s\nyOld: %s\nyNew: %s\nyWiden: %s\nstack: %s\ncache: %s\ncache': %s\n\n" (show a) (show bOld) (show b) (show bNew) (show (H.toList xs)) (show cache) (show cache'))
                  (Iterate,cache')

      -- We are in an unstable fixpoint component, but not at its head. This means, we have to wait until the head stabilized.
      | otherwise =
        let cache' = cache { componentHead = H.delete a (componentHead cache),
                             componentBody = H.insert a (componentBody cache),
                             store = M.insert a (b,Instable) (store cache) }
        in -- Debug.trace (printf "update inside\nx: %s\nyNew: %s\nstack: %s\ncache: %s\ncache': %s\n\n" (show a) (show b) (show (H.toList xs)) (show cache) (show cache'))
           (Return b,cache')



-- Helper Types & Functions ---------------------------------------
data Unit a b = Unit

instance IsEmpty (Unit a b) where
  empty = Unit

newtype Const c a' b' a b = Const { getConst :: c a' b' }

instance IsEmpty (c a' b') => IsEmpty (Const c a' b' a b) where
  empty = Const empty

data Cache a b = Cache
  { store :: HashMap a (b,Stable)
  , componentHead :: HashSet a
  , componentBody :: HashSet a
  }

instance (Show a, Show b) => Show (Cache a b) where
  show cache = printf "{store: %s, componentHead: %s, componentBody: %s}"
               (show (M.toList (store cache)))
               (show (H.toList (componentHead cache)))
               (show (H.toList (componentBody cache)))

instance (Show a, Show b, Identifiable a, LowerBounded b) => IsEmpty (Cache a b) where
  empty = Cache { store = M.empty, componentHead = H.empty, componentBody = H.empty }


instance (Show a,Show b) => Show (Compute cache a b) where
  show (Compute a) = printf "Compute %s" (show a)
  show (ComputeAndIterate a _) = printf "ComputeAndIterate %s" (show a)
  show (Cached a b) = printf "Cached %s %s" (show a) (show b)

map :: (a -> a') -> (Update cache a b -> Update cache' a' b) -> Compute cache a b -> Compute cache' a' b
map f g c = case c of
  Compute a -> Compute (f a)
  ComputeAndIterate a upd -> ComputeAndIterate (f a) (g upd)
  Cached a b -> Cached (f a) b

map' :: (a -> a') -> (cache a b -> cache' a' b) -> (cache' a' b -> cache a b) -> Compute cache a b -> Compute cache' a' b
map' f g h = map f (\upd b c -> let (i,c') = upd b (h c) in (i,g c'))
