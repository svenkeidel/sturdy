{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Transformer.Abstract.Fix.Parallel where

import           Prelude hiding (pred,lookup,map,head,iterate,(.))

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Fix
import           Control.Arrow.State
import           Control.Arrow.Reader
import           Control.Arrow.Trans
import           Control.Arrow.Order(ArrowComplete(..),ArrowJoin(..))
import           Control.Arrow.Transformer.Reader
import           Control.Arrow.Transformer.State

import           Data.Profunctor
import           Data.Order
import qualified Data.HashSet as H
import           Data.Identifiable
import           Data.Empty
import           Data.Coerce

import           Data.Abstract.Cache(IsCache,Cache)
import qualified Data.Abstract.Cache as Cache
import           Data.Abstract.StackWidening(Stack(..))
import           Data.Abstract.Widening(Stable(..))
import qualified Data.Abstract.Widening as W

data Iteration cache a b = Iteration { old :: cache a b, new :: cache a b, stable :: !Stable }
newtype ParallelT a b c x y = ParallelT (ReaderT (Stack a) (StateT (Iteration Cache a b) c) x y)
  deriving (Profunctor,Category,Arrow,ArrowChoice)

runParallelT :: (Profunctor c) => ParallelT a b c x y -> c x (Cache a b,y)
runParallelT (ParallelT f) = dimap (\a -> (empty,(empty,a))) (first new) (runStateT (runReaderT f))

execParallelT :: (Profunctor c) => ParallelT a b c x y -> c x (Cache a b)
execParallelT f = rmap fst (runParallelT f)

evalParallelT :: (Profunctor c) => ParallelT a b c x y -> c x y
evalParallelT f = rmap snd (runParallelT f)

parallel :: (Identifiable a, LowerBounded b, Profunctor c, ArrowChoice c) => Cache.Widening (Iteration Cache) a b -> IterationStrategy (ParallelT a b c) a b
parallel widen (ParallelT f) = ParallelT $ push $ proc (xs,a) -> do
  cache <- get -< ()
  case Cache.lookup a cache of
    Just (_,b) | H.member a xs -> returnA -< b
    _ -> iterate -< (xs,a)
  where
    iterate = proc (xs,a) -> do
      modify' (\(a,cache) -> ((),Cache.initialize a bottom Instable cache)) -< a
      b <- f -< a
      (st,b') <- modify' (\((a,b),cache) -> Cache.update widen a b cache) -< (a,b)
      cache <- get -< ()
      if not (H.null xs) || st == Stable
      then returnA -< b'
      else do
        put -< cache { new = empty, old = new cache, stable = Stable }
        iterate -< (xs,a)
    
    push g = proc a -> do
      Stack xs <- ask -< ()
      local g -< (Stack (H.insert a xs),(xs,a))

instance IsCache cache a b => IsCache (Iteration cache) a b where
  type Widening (Iteration cache) a b = Cache.Widening cache a b
  lookup a cache = do
    (st,b) <- Cache.lookup a (new cache)
    return (st ⊔ stable cache,b)
  initialize a b st cache =
    case Cache.lookup a (old cache) of
      Just (st',b') -> 
        let new' = Cache.initialize a b' st' (new cache)
        in cache { new = new', stable = st' ⊔ stable cache }
      Nothing ->
        let new' = Cache.initialize a b st (new cache)
        in cache { new = new', stable = Instable }
  update widen a b cache =
    let ((st,b'),new') = Cache.update widen a b (new cache)
        st' = st ⊔ stable cache
    in ((st',b'),cache { new = new', stable = st' })

instance (ArrowRun c) => ArrowRun (ParallelT a b c) where
  type Rep (ParallelT a b c) x y = Rep c x y
  run = run . evalParallelT
  {-# INLINE run #-}

type instance Fix x y (ParallelT _ _ c) = ParallelT x y c
instance (Profunctor c,ArrowApply c) => ArrowApply (ParallelT a b c) where app = ParallelT (lmap (first coerce) app)
instance IsEmpty (cache a b) => IsEmpty (Iteration cache a b) where empty = Iteration empty empty W.Stable
instance (Profunctor c,Arrow c) => ArrowJoin (ParallelT a b c) where
  join _lub (ParallelT f) (ParallelT g) = ParallelT $ rmap (uncurry _lub) (f &&& g)
instance (Profunctor c,Arrow c, Complete y) => ArrowComplete y (ParallelT a b c) where
  ParallelT f <⊔> ParallelT g = ParallelT $ rmap (uncurry (⊔)) (f &&& g)



