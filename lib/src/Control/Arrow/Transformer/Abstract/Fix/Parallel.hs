{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Transformer.Abstract.Fix.Parallel where

-- import           Prelude hiding (pred,lookup,map,head,iterate,(.))

-- import           Control.Category
-- import           Control.Arrow
-- import           Control.Arrow.Fix
-- import           Control.Arrow.State
-- import           Control.Arrow.Reader
-- import           Control.Arrow.Trans
-- import           Control.Arrow.Cache
-- import           Control.Arrow.Order(ArrowComplete(..),ArrowJoin(..),ArrowEffectCommutative)
-- import           Control.Arrow.Transformer.Reader
-- import           Control.Arrow.Transformer.State

-- import           Data.Profunctor
-- import           Data.Order
-- import qualified Data.HashSet as H
-- import           Data.Identifiable
-- import           Data.Empty
-- import           Data.Coerce
-- import           Data.HashMap.Lazy(HashMap)
-- import qualified Data.HashMap.Lazy as M

-- import           Data.Abstract.StackWidening(Stack(..))
-- import           Data.Abstract.Widening(Stable(..))
-- import qualified Data.Abstract.Widening as W

-- data Iteration a b = Iteration { old :: HashMap a b, new :: HashMap a b, stable :: Stable }
-- newtype ParallelT a b c x y = ParallelT (StateT (Iteration a b) c x y)
--   deriving (Profunctor,Category,Arrow,ArrowChoice)

-- runParallelT :: (Profunctor c) => ParallelT cache a b c x y -> c x (HashMap a b,y)
-- runParallelT (ParallelT f) = dimap (\a -> (empty,a)) (first new) (runStateT f)

-- parallel :: (Identifiable a, Profunctor c, ArrowChoice c) => W.Widening b -> IterationStrategy (ParallelT a b c) a b
-- parallel widen (ParallelT f) = ParallelT $ push $ memoize $ proc (r,a) -> do
--   case r of
--     Just (_,b) | H.member a xs -> returnA -< b
--     _ -> iterate -< (xs,a)
--   where
--     iterate = proc (xs,a) -> do
--       modify' (\(a,cache) -> ((),Cache.initialize a cache)) -< a
--       b <- f -< a
--       (st,b') <- modify' (\((a,b),cache) -> Cache.update widen a b cache) -< (a,b)
--       cache <- get -< ()
--       if H.null xs && st == Instable
--       then do
--         put -< cache { new = empty, old = new cache, stable = Stable }
--         iterate -< (xs,a)
--       else
--         returnA -< b'
    
--     push g = proc a -> do
--       Stack xs <- ask -< ()
--       local g -< (Stack (H.insert a xs),(xs,a))

-- initialize a cache =
--   case (M.lookup a (old cache), M.lookup a (new cache)) of
--     (Just b, Nothing) -> 
--       let new' = M.insert a b (new cache)
--       in cache { new = new', stable = stable cache ⊔ st }
--     (Nothing, Nothing) ->
--       let new' = M.insert a bottom (new cache)
--       in cache { new = new', stable = Instable }
--     (_,_) -> cache

-- instance ArrowCache Iteration a b where
--   insert a b st cache = cache { new = Cache.insert a b st (new cache)
--                               , stable = stable cache ⊔ st }
--   setStable a cache = cache { new = Cache.setStable a (new cache)}
--   update widen a b cache =
--     let ((st,b'),new') = Cache.update widen a b (new cache)
--         st' = stable cache ⊔ st  
--     in ((st',b'),cache { new = new', stable = st' })
--   lookup a cache = do
--     (st,b) <- Cache.lookup a (new cache)
--     return (stable cache ⊔ st, b)

-- instance (IsCache cache a b, ArrowRun c) => ArrowRun (ParallelT cache a b c) where
--   type Rep (ParallelT cache a b c) x y = Rep c x (cache a b, y)
--   run = run . runParallelT
--   {-# INLINE run #-}

-- instance (Profunctor c,ArrowApply c) => ArrowApply (ParallelT cache a b c) where app = ParallelT (lmap (first coerce) app)
-- instance IsEmpty (cache a b) => IsEmpty (Iteration cache a b) where empty = Iteration empty empty W.Stable
-- instance (ArrowEffectCommutative c) => ArrowJoin (ParallelT cache a b c) where
--   joinSecond (ParallelT g) = ParallelT $ second g
-- instance (ArrowEffectCommutative c, Complete y) => ArrowComplete y (ParallelT cache a b c) where
--   ParallelT f <⊔> ParallelT g = ParallelT $ rmap (uncurry (⊔)) (f &&& g)
-- instance ArrowEffectCommutative c => ArrowEffectCommutative (ParallelT cache a b c)


