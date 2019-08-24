{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}
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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Abstract.StackWidening where

import           Prelude hiding (pred,lookup)

import           Data.Order
import           Data.Measure
import           Data.Hashable(Hashable(..))
import           Data.HashMap.Lazy(HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.HashSet(HashSet)
import qualified Data.HashSet as H
import           Data.Identifiable
import           Data.Maybe
import           Data.Empty
import           Data.Monoidal
import           Data.Bifunctor as B
import           Data.Ord(comparing)
import           Data.Foldable(minimumBy)

import           Data.Abstract.Widening(Widening,Stable(..))

import qualified Debug.Trace as Debug

import           Text.Printf

-- | A stack widening operator @(▽ :: s -> a -> (s,a))@ follows the same
-- idea as a regular widening operator, but does not have the
-- restriction that the input chain has to be ascending:
-- Given an infinite series @x1, x2, x3, x4 ...@, the stack widening
-- operator produces a series
-- @
--   x1 ▽ s1 = (x1',s2)  &  x1 ⊑ x1'
--   x2 ▽ s2 = (x2',s3)  &  x2 ⊑ x2'
--   x3 ▽ s3 = (x3',s4)  &  x3 ⊑ x3'
--   x4 ▽ s4 = (x4',s5)  &  x4 ⊑ x4'
--   ...
-- @
-- such that @x1', x2', x3' ...@ repeats itself, i.e.  there exists @n,m@
-- with @n /= m@ and @xn' = xm'@.
type StackWidening stack a = a -> stack a -> ((a,Loop),stack a)
data Loop = Loop | NoLoop deriving (Eq,Show)

trace :: (Eq a, Show (s a), Show a) => StackWidening s a -> StackWidening s a
trace strat a s =
  let r@((a',l),s') = strat a s
  in if a /= a' || l == Loop
     then Debug.trace (printf "STACKWIDEN -- %s\n\tx: %s -> %s\n\tstack: %s -> %s\n\n" (show l) (show a) (show a')  (show s) (show s')) r
     else r
{-# NOINLINE trace #-}

finite :: StackWidening Unit a
finite a sc = ((a,NoLoop),sc)
{-# INLINE finite #-}

groupBy :: forall k a' a stack. (IsEmpty (stack a'), Identifiable k)
        => Iso (->) a (k,a') -> StackWidening stack a' -> StackWidening (Groups stack k a') a
groupBy iso strat a stacks =
  let (k,a') = from iso a
      s = lookup k stacks
      (r,s') = strat a' s
  in (B.first (\a'' -> to iso (k,a'')) r, insert k s' stacks)
  where
    lookup k (Groups cs) = fromMaybe empty (M.lookup k cs)
    insert k c (Groups cs) = Groups (M.insert k c cs)
    {-# INLINE lookup #-}
    {-# INLINE insert #-}
{-# INLINE groupBy #-}

maxSize :: Int -> StackWidening s a -> StackWidening (Size ** s) a
maxSize limit strat a (Product (Size n) s)
  | n+1 < limit = incSize ((a,NoLoop),s)
  | otherwise = incSize (strat a s)
  where
    incSize (r,s') = (r,Product (Size (n+1)) s')
    {-# INLINE incSize #-}
{-# INLINE maxSize #-}

fromWidening :: Complete a => Widening a -> StackWidening Last a
fromWidening w a (Last m) =
  let (st,a') = case m of
        Nothing -> (Instable,a)
        Just x  -> x `w` (x ⊔ a)
  in case st of
       Stable   -> ((a',Loop), Last (Just a'))
       Instable -> ((a',NoLoop), Last (Just a'))
{-# INLINE fromWidening #-}

reuse :: Identifiable a => (a -> HashSet a -> Maybe a) -> StackWidening s a -> StackWidening (Stack ** s) a
reuse pred strat a (Product (Stack xs) s) = case pred a xs of
  Just a' -> ((a',Loop), Product (Stack xs) s)
  Nothing ->
    let ((a',l),s') = strat a s
    in ((a',l), Product (Stack (H.insert a' xs)) s')
{-# INLINE reuse #-}

detectLoop :: Identifiable a => StackWidening Stack a
detectLoop a0 s =
  let (a',Product s' Unit) = reuse (\a xs -> if H.member a xs then Just a else Nothing) finite a0 (Product s Unit)
  in (a',s')
{-# INLINE detectLoop #-}

-- | Reuses elements on the stack based on a measure.
reuseMeasuredBy :: (PreOrd a, Identifiable a, Ord n) => (a -> n) -> StackWidening s a -> StackWidening (Stack ** s) a
reuseMeasuredBy measure' strat = flip reuse strat $ \a xs ->
  let upper = [ x | x <- H.toList xs, a ⊑ x ]
  in if null upper
     then Nothing
     else Just (minimumBy (comparing measure') upper)
{-# INLINE reuseMeasuredBy #-}

reuseMeasured :: (Measurable a n, Identifiable a) => StackWidening s a -> StackWidening (Stack ** s) a
reuseMeasured = reuseMeasuredBy measure
{-# INLINE reuseMeasured #-}

newtype Groups s k a' a = Groups (HashMap k (s a')) deriving (Eq,Hashable)
instance (Show k, Show (s a')) => Show (Groups s k a' a) where show (Groups m) = show (M.toList m)
instance IsEmpty (Groups c k a' a) where empty = Groups M.empty

newtype Size a = Size Int deriving (Show,Eq,Hashable)
instance IsEmpty (Size a) where empty = Size 0

newtype Stack a = Stack (HashSet a) deriving (Eq,Hashable)
instance Show a => Show (Stack a) where show (Stack l) = show (H.toList l)
instance IsEmpty (Stack a) where empty = Stack H.empty

newtype Last a = Last (Maybe a) deriving (Eq,Hashable)
instance Show a => Show (Last a) where show (Last m) = show m
instance IsEmpty (Last a) where empty = Last Nothing

data (s1 ** s2) a = Product (s1 a) (s2 a) deriving (Eq)
instance (Show (s1 a), Show (s2 a)) => Show ((s1 ** s2) a) where
  show (Product s1 s2) = printf "%s ** %s" (show s1) (show s2)
instance (IsEmpty (s1 a), IsEmpty (s2 a)) => IsEmpty ((s1 ** s2) a) where
  empty = Product empty empty
instance (Hashable (s1 a), Hashable (s2 a)) => Hashable ((s1 ** s2) a) where
  hashWithSalt s (Product s1 s2) = s `hashWithSalt` (s1,s2)
  {-# INLINE hashWithSalt #-}

newtype Const s a' a = Const { getConst :: s a' } deriving (Eq,Hashable)
instance Show (s a') => Show (Const s a' a) where show (Const s) = show s
instance (IsEmpty (s a')) => IsEmpty (Const s a' a) where empty = Const empty

data Unit a = Unit deriving (Show,Eq)
instance IsEmpty (Unit a) where
  empty = Unit

instance Hashable (Unit a) where
  hashWithSalt s Unit = s `hashWithSalt` (1 :: Int)
  {-# INLINE hashWithSalt #-}

