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
module Data.Abstract.StackWidening where

import           Prelude hiding (pred,lookup)

import           Data.Order
import           Data.HashMap.Lazy(HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.HashSet(HashSet)
import qualified Data.HashSet as H
import           Data.Identifiable
import           Data.Maybe
import           Data.Abstract.Widening(Widening)
import           Data.Lens(Iso',get,from)
import           Data.Utils (maybeHead)
import           Data.Empty

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

type StackWidening stack a = a -> stack a -> (a,stack a)

finite :: StackWidening Unit a
finite a sc = (a,sc)

groupBy :: (IsEmpty (stack a'), Identifiable k)
        => Iso' a (k,a') -> StackWidening stack a' -> StackWidening (Groups stack k a') a
groupBy iso strat a stacks =
  let (k,a') = get iso a
      s = lookup k stacks
      (a'',s') = strat a' s
  in (get (from iso) (k,a''), insert k s' stacks)
  where
    lookup k (Groups cs) = fromMaybe empty (M.lookup k cs)
    insert k c (Groups cs) = Groups (M.insert k c cs)

maxSize :: Int -> StackWidening s a -> StackWidening (Size ** s) a
maxSize limit strat a (Product (Size n) s)
  | n+1 < limit = incSize (a,s)
  | otherwise = incSize (strat a s)
  where
    incSize (r,s') = (r,Product (Size (n+1)) s')

fromWidening :: Complete a => Widening a -> StackWidening s a -> StackWidening (Last ** s) a
fromWidening w strat a (Product (Last m) s) =
  let a' = case m of
             Nothing -> a
             Just x  -> let (_,x') = x `w` (x ⊔ a) in x'
      (a'',s') = strat a' s
  in (a'',Product (Last (Just a'')) s')


reuse :: Identifiable a => (a -> HashSet a -> Maybe a) -> StackWidening s a -> StackWidening (Stack ** s) a
reuse choose strat a (Product (Stack xs) s) =
  let (a',s') = case choose a xs of
                  Just x' -> strat x' s
                  Nothing -> strat a s
  in (a',Product (Stack (H.insert a' xs)) s')


reuseFirst :: (Identifiable a, PreOrd a) => StackWidening s a -> StackWidening (Stack ** s) a
reuseFirst = reuse (\a l -> maybeHead [x | x <- H.toList l, a ⊑ x ])


data Groups s k a' a = Groups (HashMap k (s a'))
instance (Show k, Show (s a')) => Show (Groups s k a' a) where show (Groups m) = show (M.toList m)
instance (Identifiable k) => IsEmpty (Groups c k a' a) where empty = Groups M.empty

data Size a = Size Int deriving (Show)
instance IsEmpty (Size a) where empty = Size 0

data Stack a = Stack (HashSet a)
instance Show a => Show (Stack a) where show (Stack l) = show (H.toList l)
instance IsEmpty (Stack a) where empty = Stack H.empty

newtype Last a = Last (Maybe a)
instance Show a => Show (Last a) where show (Last m) = show m
instance IsEmpty (Last a) where empty = Last Nothing

data (s1 ** s2) a = Product (s1 a) (s2 a)
instance (Show (s1 a), Show (s2 a)) => Show ((s1 ** s2) a) where
  show (Product s1 s2) = printf "%s ** %s" (show s1) (show s2)
instance (IsEmpty (s1 a), IsEmpty (s2 a)) => IsEmpty ((s1 ** s2) a) where
  empty = Product empty empty

newtype Const s a' a = Const { getConst :: s a' }
instance Show (s a') => Show (Const s a' a) where show (Const s) = show s
instance (IsEmpty (s a')) => IsEmpty (Const s a' a) where empty = Const empty

data Unit a = Unit deriving (Show)
instance IsEmpty (Unit a) where
  empty = Unit

