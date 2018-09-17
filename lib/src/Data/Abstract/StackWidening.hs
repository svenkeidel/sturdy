{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
module Data.Abstract.StackWidening where

import           Control.Monad.State

import           Data.Order
import           Data.HashMap.Lazy(HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.Identifiable
import           Data.Monoidal
import           Data.Maybe
import           Data.Abstract.Widening(Widening)

-- | A stack widening operator (▽ :: s -> a -> (s,a)) follows the same
-- idea as a regular widening operator, but does not have the
-- restriction that the input chain has to be ascending:
-- Given an infinite series x1, x2, x3, x4 ..., the stack widening
-- operator produces a series
-- s1 ▽ x1 = (s2,x1')  &  x1 ⊑ x1'
-- s2 ▽ x2 = (s3,x2')  &  x2 ⊑ x2'
-- s3 ▽ x3 = (s4,x3')  &  x3 ⊑ x3'
-- s4 ▽ x4 = (s5,x4')  &  x4 ⊑ x4'
-- ...
-- such that x1', x2', x3' ... repeats itself, i.e.  there exists n,m
-- with n /= m and xn' = xm'.
type StackWidening s a = a -> State (s a) a

data Unit a = Unit
instance Monoid (Unit a) where
  mempty = Unit
  mappend _ _ = Unit

-- | Trivial stack widening if the abstract domain is finite.
finite :: StackWidening Unit a
finite a = return a

data Stack a = Stack Int [a]
instance Monoid (Stack a) where
  mempty = Stack 0 []
  mappend (Stack n st) (Stack n' st') = Stack (n+n') (st ++ st')

-- | Pushes elements onto a stack and increases its size. Always calls
-- the given widening.
stack :: StackWidening Stack a -> StackWidening Stack a
stack f x = state $ \s@(Stack n st) -> let x' = evalState (f x) s in (x',Stack (n+1) (x':st))

-- | Return the same elements until the specified maximum stack size
-- is reached, then call the fallback widening.
maxSize :: Int -> StackWidening Stack a -> StackWidening Stack a
maxSize limit fallback x = do
  Stack n _ <- get
  if n <= limit
  then return x
  else fallback x

-- | Reuse an element from the stack that is greater than the current
-- element. If no such elements exist, call the fallback widening.
reuse :: (Show a, PreOrd a) => (a -> [a] -> a) -> StackWidening Stack a -> StackWidening Stack a
reuse bestChoice fallback x = do
  Stack _ st <- get
  -- All elements in the stack that are greater than the current
  -- element are potential candidates.
  let candidates = [ y | y <- st, x ⊑ y ]
  if | null st               -> return x
     | not (null candidates) -> return $ bestChoice x candidates
     | otherwise             -> fallback x

data Categories k b s a = Categories (HashMap k (s b))
instance (Identifiable k, Monoid (s b)) => Monoid (Categories k b s a) where
  mempty = Categories M.empty
  mappend (Categories m) (Categories m') = Categories (m `mappend` m')

categorize :: (Monoid (s b), Identifiable k) => Iso a (k,b) -> StackWidening s b -> StackWidening (Categories k b s) a
categorize iso w a = do
  Categories m <- get
  let (k,b) = to iso a
      s = fromMaybe mempty (M.lookup k m)
      (b',s') = runState (w b) s
  put $ Categories $ M.insert k s' m
  return (from iso (k,b'))

fromWidening :: Complete a => Widening a -> StackWidening Stack a
fromWidening w a = do
  Stack _ s <- get
  case s of
    [] -> return a
    x:_ -> do
      let x' = a `w` (x ⊔ a)
      return x'

data Product s1 s2 x where
  Product :: s1 a -> s2 b -> Product s1 s2 (a,b)
    
(**) :: StackWidening s1 a -> StackWidening s2 b -> StackWidening (Product s1 s2) (a,b)
(**) f g (a,b) = do
  Product s1 s2 <- get
  let (a',s1') = runState (f a) s1
      (b',s2') = runState (g b) s2 
  put $ Product s1' s2'
  return (a',b')

topOut :: UpperBounded a => StackWidening s a
topOut _ = return top
