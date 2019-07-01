{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Abstract.IterationStrategy where

import           Prelude hiding (pred,lookup,map,head,iterate,(.))

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Reader
import           Control.Arrow.Abstract.Join
import           Control.Arrow.Transformer.Reader
import           Control.Arrow.Transformer.State
import           Control.Arrow.Transformer.Writer

import           Data.Profunctor
import           Data.Order
import           Data.HashMap.Lazy(HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.HashSet(HashSet)
import qualified Data.HashSet as H
import           Data.Identifiable
import           Data.Lens (Prism',getMaybe,set)
import           Data.Empty
import           Data.Maybe
import           Data.Coerce

import           Data.Abstract.StackWidening(StackWidening,Stack(..))
import           Data.Abstract.Widening(Widening,Stable(..))

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

type IterationStrategy c a b = c a b -> c a b

class ArrowRun t where
  run :: Profunctor c => t c a b -> c a b

-- data Unit a b = Unit

-- finite :: IterationStrategy (->) a b
-- finite a = Compute a

-- trace :: (Show a, Show b, Show (stack a), Show (cache a b))
--       => IterationStrategy stack cache a b -> IterationStrategy stack cache a b
-- trace strat a (s,c) =
--   let (r,(s',c')) = strat a (s,c)
--       r' = map id (\upd b ca ->
--                      let (i,ca') = upd b ca
--                      in Debug.trace (printf "UPDATE\nx: %s\ny: %s\niterate: %s\nstack: %s\ncache: %s\ncache': %s\n\n"
--                           (show a) (show b) (show i) (show s) (show ca) (show ca')) (i,ca')) r
--   in Debug.trace (printf "CALL\nx: %s\nstrat: %s\nstack: %s\ncache: %s\nstack: %s\ncache': %s\n\n"
--                  (show a) (show r) (show s) (show c) (show s') (show c')) (r',(s',c'))


-- newtype Const c a' b' a b = Const { getConst :: c a' b' }Stack.Unit Unit

filter :: (Profunctor c, ArrowChoice c, ArrowApply c) => Prism' a a' -> IterationStrategy c a' b -> IterationStrategy c a b
filter pred strat f = proc a -> case getMaybe pred a of
  Just a' ->
    strat (lmap (\x -> set pred x a) f) -<< a'
  Nothing -> do
    f -< a


data Cache a b = Cache { old :: HashMap a b, new :: HashMap a b }
newtype ParallelT s a b c x y = ParallelT (StateT (Cache a b) (ReaderT (Stack a,s a) c) x y) deriving (Profunctor,Category,Arrow,ArrowChoice)
instance (Profunctor c,ArrowApply c) => ArrowApply (ParallelT s a b c) where app = ParallelT (lmap (first coerce) app)
instance IsEmpty (Cache a b) where empty = Cache M.empty M.empty
instance (Profunctor c,Arrow c) => ArrowJoin (ParallelT s a b c) where
  joinWith _lub (ParallelT f) (ParallelT g) = ParallelT $ rmap (uncurry _lub) (f &&& g)
instance (IsEmpty (s a)) => ArrowRun (ParallelT s a b) where
  run (ParallelT f) = dimap (\a -> (empty,(empty,a))) snd (runReaderT (runStateT f))


parallel :: (Identifiable a, LowerBounded b, Profunctor c, ArrowChoice c) => StackWidening s a -> Widening b -> IterationStrategy (ParallelT s a b c) a b
parallel stackWiden widen (ParallelT (StateT f)) = ParallelT $ StateT $ stackWidening $ proc (xs,cache,a) -> case M.lookup a (new cache) of
  Just b | H.member a xs -> returnA -< (cache,b)
  _ -> iterate -< (xs,cache,a)
  where
    iterate = proc (xs,cache,a) -> do
      let bold = fromMaybe bottom (M.lookup a (old cache))
      (cache',b) <- f -< (cache { new = M.insertWith (\_ o -> o) a bold (new cache) },a)
      let cache'' = cache' {new = M.insertWith (\bNew bOld -> snd (widen bOld bNew)) a b (new cache')}
      if not (H.null xs) || new cache'' ⊑ old cache''
      then returnA -< (cache'',b)
      else iterate -< (xs,cache'' { new = M.empty, old = new cache'' },a)
    
    stackWidening g = proc (cache,a) -> do
      (Stack xs,s2) <- ask -< ()
      let (a',s2') = stackWiden a s2
      local g -< ((Stack (H.insert a' xs),s2'),(xs,cache,a'))

data Component a = Component { head :: HashSet a, body :: HashSet a }
instance Identifiable a => Semigroup (Component a) where
  (<>) = mappend
instance Identifiable a => Monoid (Component a) where
  mempty = Component { head = H.empty, body = H.empty }
  c1 `mappend` c2 = Component { head = head c1 <> head c2, body = body c1 <> body c2 }

newtype ChaoticT s a b c x y = ChaoticT (WriterT (Component a) (StateT (HashMap a (b,Stable)) (ReaderT (Stack a,s a) c)) x y) deriving (Profunctor,Category,Arrow,ArrowChoice)
instance (Identifiable a,Profunctor c,ArrowApply c) => ArrowApply (ChaoticT s a b c) where app = ChaoticT (lmap (first coerce) app)
instance (Identifiable a,Profunctor c,Arrow c) => ArrowJoin (ChaoticT s a b c) where
  joinWith _lub (ChaoticT f) (ChaoticT g) = ChaoticT $ rmap (uncurry _lub) (f &&& g)
instance (IsEmpty (s a)) => ArrowRun (ChaoticT s a b) where
  run (ChaoticT f) = dimap (\a -> (empty,(M.empty,a))) (snd . snd) (runReaderT (runStateT (runWriterT f)))

chaotic :: (Identifiable a, LowerBounded b, Profunctor c, ArrowChoice c, ArrowApply c) => StackWidening s a -> Widening b -> IterationStrategy (ChaoticT s a b c) a b
chaotic stackWiden widen (ChaoticT (WriterT (StateT f))) = ChaoticT $ WriterT $ StateT $ stackWidening $ proc (stack,cache,a) -> do
  case M.lookup a cache of
    Just (b,Stable) ->
      returnA -< (cache,(mempty,b))

    Just (b,Instable) | H.member a stack -> do
      returnA -< (cache,(Component {head = H.singleton a, body = H.empty},b))

    _ ->
      iterate -<< (M.insertWith (\_ o -> o) a (bottom,Instable) cache,a)

  where
    iterate = proc (cache,a) -> do
      (cache',(component,b)) <- f -< (cache,a)

      case () of
        -- The call did not depend on any unstable calls. This means we are done and don't need to iterate.
        () | H.null (head component) -> do
             returnA -< (M.insert a (b,Stable) cache',(mempty,b))

        -- We are at the head of a fixpoint component. This means, we have to iterate until the head stabilized.
           | head component == H.singleton a -> do
             let (bOld,_) = M.lookupDefault (bottom,Instable) a cache
                 (stable,bNew) = widen bOld b
             
             let cache'' = M.insert a (bNew,stable) cache'
             case stable of
               -- If the head of a fixpoint component is stable, flag all elements in the body of the component as stable too and return.
               Stable -> do
                 returnA -< (foldr (M.adjust (second (\_ -> Stable))) cache'' (body component),(mempty,bNew))

               -- If the head of a fixpoint component is not stable, keep iterating.
               Instable ->
                 iterate -<< (cache'',a)

        -- We are in an unstable fixpoint component, but not at its head. This means, we have to wait until the head stabilized.
           | otherwise -> do
             returnA -< (M.insert a (b,Instable) cache,(Component {head = H.delete a (head component), body = H.insert a (body component)}, b)) 

    stackWidening g = proc (cache,a) -> do
      (Stack xs,s2) <- ask -< ()
      let (a',s2') = stackWiden a s2
      local g -< ((Stack (H.insert a' xs),s2'),(xs,cache,a'))
