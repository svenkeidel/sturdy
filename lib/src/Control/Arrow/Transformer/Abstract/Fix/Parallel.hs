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
import           Control.Arrow.Order(ArrowComplete(..),ArrowJoin(..),ArrowEffectCommutative)
import           Control.Arrow.Transformer.Reader
import           Control.Arrow.Transformer.State

import           Data.Profunctor
import           Data.Order
import qualified Data.HashSet as H
import           Data.Identifiable
import           Data.Empty
import           Data.Coerce

import           Data.Abstract.Cache(IsCache)
import qualified Data.Abstract.Cache as Cache
import           Data.Abstract.StackWidening(Stack(..))
import           Data.Abstract.Widening(Stable(..))
import qualified Data.Abstract.Widening as W

data Iteration cache a b = Iteration { old :: cache a b, new :: cache a b, stable :: Stable }
newtype ParallelT cache a b c x y = ParallelT (ReaderT (Stack a) (StateT (Iteration cache a b) c) x y)
  deriving (Profunctor,Category,Arrow,ArrowChoice)

runParallelT :: (IsCache cache a b, Profunctor c) => ParallelT cache a b c x y -> c x (cache a b,y)
runParallelT (ParallelT f) = dimap (\a -> (empty,(empty,a))) (first new) (runStateT (runReaderT f))

execParallelT :: (IsCache cache a b, Profunctor c) => ParallelT cache a b c x y -> c x (cache a b)
execParallelT f = rmap fst (runParallelT f)

evalParallelT :: (IsCache cache a b, Profunctor c) => ParallelT cache a b c x y -> c x y
evalParallelT f = rmap snd (runParallelT f)

parallel :: (Identifiable a, IsCache cache a b, Profunctor c, ArrowChoice c) => Cache.Widening (Iteration cache) a b -> IterationStrategy (ParallelT cache a b c) a b
parallel widen (ParallelT f) = ParallelT $ push $ proc (xs,a) -> do
  cache <- get -< ()
  case Cache.lookup a cache of
    Just (_,b) | H.member a xs -> returnA -< b
    _ -> iterate -< (xs,a)
  where
    iterate = proc (xs,a) -> do
      modify' (\(a,cache) -> ((),Cache.initialize a cache)) -< a
      b <- f -< a
      (st,b') <- modify' (\((a,b),cache) -> Cache.update widen a b cache) -< (a,b)
      cache <- get -< ()
      if H.null xs && st == Instable
      then do
        put -< cache { new = empty, old = new cache, stable = Stable }
        iterate -< (xs,a)
      else
        returnA -< b'
    
    push g = proc a -> do
      Stack xs <- ask -< ()
      local g -< (Stack (H.insert a xs),(xs,a))

instance IsCache cache a b => IsCache (Iteration cache) a b where
  type Widening (Iteration cache) a b = Cache.Widening cache a b
  initialize a cache =
    case (Cache.lookup a (old cache), Cache.lookup a (new cache)) of
      (Just (st,b), Nothing) -> 
        let new' = Cache.insert a b st (new cache)
        in cache { new = new', stable = stable cache ⊔ st }
      (Nothing, Nothing) ->
        let new' = Cache.initialize a (new cache)
        in cache { new = new', stable = Instable }
      (_,_) -> cache
  insert a b st cache = cache { new = Cache.insert a b st (new cache)
                              , stable = stable cache ⊔ st }
  setStable a cache = cache { new = Cache.setStable a (new cache)}
  update widen a b cache =
    let ((st,b'),new') = Cache.update widen a b (new cache)
        st' = stable cache ⊔ st  
    in ((st',b'),cache { new = new', stable = st' })
  lookup a cache = do
    (st,b) <- Cache.lookup a (new cache)
    return (stable cache ⊔ st, b)

instance (IsCache cache a b, ArrowRun c) => ArrowRun (ParallelT cache a b c) where
  type Rep (ParallelT cache a b c) x y = Rep c x (cache a b, y)
  run = run . runParallelT
  {-# INLINE run #-}

instance (Profunctor c,ArrowApply c) => ArrowApply (ParallelT cache a b c) where app = ParallelT (lmap (first coerce) app)
instance IsEmpty (cache a b) => IsEmpty (Iteration cache a b) where empty = Iteration empty empty W.Stable
instance (ArrowEffectCommutative c) => ArrowJoin (ParallelT cache a b c) where
  join _lub (ParallelT f) (ParallelT g) = ParallelT $ rmap (uncurry _lub) (f &&& g)
instance (ArrowEffectCommutative c, Complete y) => ArrowComplete y (ParallelT cache a b c) where
  ParallelT f <⊔> ParallelT g = ParallelT $ rmap (uncurry (⊔)) (f &&& g)
instance ArrowEffectCommutative c => ArrowEffectCommutative (ParallelT cache a b c)


