{-# LANGUAGE Arrows #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Fix.Stack where

import Prelude hiding (elem)
import Control.Arrow
import Control.Arrow.Fix
import Control.Arrow.Trans
import Control.Arrow.Transformer.Const
import Control.Arrow.Transformer.Reader
import Control.Arrow.Transformer.State
import Control.Arrow.Transformer.Static
import Control.Arrow.Transformer.Writer

import Data.Profunctor
import Data.Order
import Data.Metric
import Data.Abstract.Widening
import Data.Maybe
import Data.Monoidal

import Text.Printf

type StackPointer = Int
data RecurrentCall = RecurrentCall StackPointer | NoLoop

class (Arrow c, Profunctor c) => ArrowStack a c | c -> a where
  push :: c x y -> c (a, x) (y)
  elem :: c a RecurrentCall

  default elem :: (c ~ t c', ArrowTrans t, ArrowStack a c') => c a RecurrentCall
  elem = lift' elem
  {-# INLINE elem #-}

push' :: ArrowStack a c => c a b -> c a b
push' f = lmap (\a -> (a,a)) (push f)
{-# INLINE push' #-}

class (Arrow c, Profunctor c) => ArrowStackDepth c where
  depth :: c () Int
  default depth :: (c ~ t c', ArrowTrans t, ArrowStackDepth c') => c () Int
  depth = lift' depth
  {-# INLINE depth #-}


class (Arrow c, Profunctor c) => ArrowStackElements a c | c -> a where
  elems :: c () [a]
  peek :: c () (Maybe a)

  default elems :: (c ~ t c', ArrowTrans t, ArrowStackElements a c') => c () [a]
  default peek :: (c ~ t c', ArrowTrans t, ArrowStackElements a c') => c () (Maybe a)

  elems = lift' elems
  peek = lift' peek

  {-# INLINE elems #-}
  {-# INLINE peek #-}

class (Arrow c, Profunctor c) => ArrowTopLevel c where
  topLevel :: FixpointCombinator c a b -> FixpointCombinator c a b -> FixpointCombinator c a b

  default topLevel :: (Underlying c a b ~ c' a' b', ArrowLift c, ArrowTopLevel c') => FixpointCombinator c a b -> FixpointCombinator c a b -> FixpointCombinator c a b
  topLevel stratTop stratLower f = lift $ topLevel (unlift1 stratTop) (unlift1 stratLower) (unlift f)
  {-# INLINE topLevel #-}

maxDepth :: (ArrowChoice c, ArrowStackDepth c) => Int -> FixpointCombinator c a b -> FixpointCombinator c a b
maxDepth limit strat f = proc a -> do
  n <- depth -< ()
  if n < limit
  then f -< a
  else strat f -< a
{-# INLINABLE maxDepth #-}

widenInput :: (Complete a, ArrowStackElements a c) => Widening a -> FixpointCombinator c a b
widenInput widen f = proc a -> do
  m <- peek -< ()
  f -< case m of
    Nothing -> a
    Just x  -> snd $ x `widen` (x ⊔ a)
{-# INLINE widenInput #-}

reuse :: (ArrowChoice c, ArrowStackElements a c) => (a -> [a] -> Maybe a) -> FixpointCombinator c a b
reuse select f = proc a -> do
  xs <- elems -< ()
  f -< fromMaybe a (select a xs)
{-# INLINE reuse #-}

reuseFirst :: (PreOrd a, ArrowChoice c, ArrowStackElements a c) => FixpointCombinator c a b
reuseFirst = reuse find
  where
    find a (x:xs)
      | a ⊑ x     = Just x
      | otherwise = find a xs
    find _ []     = Nothing
{-# INLINE reuseFirst #-}

reuseByMetric :: (PreOrd a, Ord n, ArrowChoice c, ArrowStackElements a c) => Metric a n -> FixpointCombinator c a b
reuseByMetric metric = reuse find
  where
    find a xs = element <$> foldMap (\a' -> if a ⊑ a' then Just (Measured a' (metric a a')) else Nothing) xs
{-# INLINE reuseByMetric #-}

data Measured a n = Measured { element :: a, measured :: n }

instance (Show a, Show n) => Show (Measured a n) where
  show m = printf "%s@%s" (show (element m)) (show (measured m))

instance Ord n => Semigroup (Measured a n) where
  m1 <> m2
    | measured m1 <= measured m2 = m1
    | otherwise                  = m2
  {-# INLINE (<>) #-}

------------- Instances --------------
instance ArrowStack a c => ArrowStack a (ConstT r c) where
  push f = lift $ \r -> push (unlift f r)
  {-# INLINE push #-}

instance ArrowStack a c => ArrowStack a (ReaderT r c) where
  push f = lift $ lmap shuffle1 (push (unlift f))
  {-# INLINE push #-}
instance ArrowStackDepth c => ArrowStackDepth (ReaderT r c)
instance ArrowStackElements a c => ArrowStackElements a (ReaderT r c)

instance ArrowStack a c => ArrowStack a (StateT s c) where
  push f = lift $ lmap shuffle1 (push (unlift f))
  {-# INLINE push #-}
instance ArrowStackDepth c => ArrowStackDepth (StateT s c)
instance ArrowStackElements a c => ArrowStackElements a (StateT s c)

instance ArrowTopLevel c => ArrowTopLevel (StateT s c)

instance (Applicative f, ArrowStack a c) => ArrowStack a (StaticT f c) where
  push (StaticT f) = StaticT $ push <$> f
  {-# INLINE push #-}
  {-# SPECIALIZE instance ArrowStack a c => ArrowStack a (StaticT ((->) r) c) #-}

instance (Monoid w, ArrowStack a c) => ArrowStack a (WriterT w c) where
  push f = lift $ push (unlift f)
  {-# INLINE push #-}

instance (Monoid w, ArrowStackElements a c) => ArrowStackElements a (WriterT w c)
instance (Monoid w, ArrowStackDepth c) => ArrowStackDepth (WriterT w c)
