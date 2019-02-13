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
import           Debug.Trace

-- | A stack widening operator @(▽ :: s -> a -> (s,a))@ follows the same
-- idea as a regular widening operator, but does not have the
-- restriction that the input chain has to be ascending:
-- Given an infinite series @x1, x2, x3, x4 ...@, the stack widening
-- operator produces a series
-- @
--   s1 ▽ x1 = (s2,x1')  &  x1 ⊑ x1'
--   s2 ▽ x2 = (s3,x2')  &  x2 ⊑ x2'
--   s3 ▽ x3 = (s4,x3')  &  x3 ⊑ x3'
--   s4 ▽ x4 = (s5,x4')  &  x4 ⊑ x4'
--   ...
-- @
-- such that @x1', x2', x3' ...@ repeats itself, i.e.  there exists @n,m@
-- with @n /= m@ and @xn' = xm'@.
--
-- Furthermore, a stack widening operator has to be monotone in the
-- second argument, i.e., for all @x ⊑ y@, @snd (s ▽ x) ⊑ snd (s ▽ y)@.
type StackWidening s a = a -> State (s a) (Loop,a)

-- | Datatype that signals that we are in a loop.
data Loop = Loop | NoLoop deriving (Show,Eq)

data Unit a = Unit deriving (Show)
instance Semigroup (Unit a) where (<>) = mappend
instance Monoid (Unit a) where
  mempty = Unit
  mappend _ _ = Unit

-- | Trivial stack widening if the abstract domain is finite.
finite :: StackWidening Unit a
finite a = return (NoLoop,a)

finite' :: StackWidening s a
finite' a = return (NoLoop,a)

data Stack a = Stack Int [a]
instance Semigroup (Stack a) where (<>) = mappend
instance Monoid (Stack a) where
  mempty = Stack 0 []
  mappend (Stack n st) (Stack n' st') = Stack (n+n') (st ++ st')
instance Show a => Show (Stack a) where show (Stack _ xs) = show xs

-- | Pushes elements onto a stack and increases its size. Always calls
-- the given widening.
stack :: StackWidening Stack a -> StackWidening Stack a
stack f x = state $ \s@(Stack n st) -> let (w,x') = evalState (f x) s in ((w,x'),Stack (n+1) (x':st))

traceStack :: Show a => StackWidening Stack a -> StackWidening Stack a
traceStack f x = state $ \s@(Stack n st) -> let (w,x') = evalState (f x) s in traceShow (w,x') ((w,x'),Stack (n+1) (x':st))

-- | Return the same elements until the specified maximum stack size
-- is reached, then call the fallback widening.
maxSize :: Int -> StackWidening Stack a -> StackWidening Stack a
maxSize limit fallback x = do
  Stack n _ <- get
  if n < limit
  then return (NoLoop,x)
  else fallback x

-- | Reuse an element from the stack that is greater than the current
-- element. If no such elements exist, call the fallback widening.
reuse :: PreOrd a => (a -> [a] -> a) -> StackWidening Stack a -> StackWidening Stack a
reuse bestChoice fallback x = do
  Stack _ st <- get
  -- All elements in the stack that are greater than the current
  -- element are potential candidates.
  let candidates = [ y | y <- st, x ⊑ y ]
  if | null st               -> return (NoLoop,x)
     | not (null candidates) -> return (Loop,bestChoice x candidates)
     | otherwise             -> fallback x

reuse' :: PreOrd a => StackWidening Stack a -> StackWidening Stack a
reuse' = reuse (\_ -> head)

data Categories k b s a = Categories (HashMap k (s b))
instance (Identifiable k, Monoid (s b)) => Semigroup (Categories k b s a) where (<>) = mappend
instance (Identifiable k, Monoid (s b)) => Monoid (Categories k b s a) where
  mempty = Categories M.empty
  mappend (Categories m) (Categories m') = Categories (m `mappend` m')
instance (Show k, Show (s b)) => Show (Categories k b s a) where
  show (Categories m) = show (M.toList m)

categorize :: (Monoid (s b), Identifiable k) => Iso a (k,b) -> StackWidening s b -> StackWidening (Categories k b s) a
categorize iso w a = do
  Categories m <- get
  let (k,b) = to iso a
      s = fromMaybe mempty (M.lookup k m)
      ((l,b'),s') = runState (w b) s
  put $ Categories $ M.insert k s' m
  return (l,from iso (k,b'))

fromWidening :: Complete a => Widening a -> StackWidening Stack a
fromWidening w a = do
  Stack _ s <- get
  case s of
    [] -> return (NoLoop,a)
    x:_ -> do
      let x' = x `w` (x ⊔ a)
      return (if (x' ⊑ x) then Loop else NoLoop,x')

data Product s1 s2 x where
  Product :: s1 a -> s2 b -> Product s1 s2 (a,b)
instance (Monoid (s1 a), Monoid (s2 b)) => Semigroup (Product s1 s2 (a,b)) where (<>) = mappend
instance (Monoid (s1 a), Monoid (s2 b)) => Monoid (Product s1 s2 (a,b)) where
  mempty = Product mempty mempty
  mappend (Product s1 s2) (Product s1' s2') = Product (s1 <> s1') (s2 <> s2')
instance (Show (s1 a), Show (s2 b)) => Show (Product s1 s2 (a,b)) where
  show (Product s1 s2) = show (s1,s2)

(**) :: StackWidening s1 a -> StackWidening s2 b -> StackWidening (Product s1 s2) (a,b)
(**) f g (a,b) = do
  Product s1 s2 <- get
  let ((la,a'),s1') = runState (f a) s1
      ((lb,b'),s2') = runState (g b) s2 
  put $ Product s1' s2'
  return (la ⊔ lb,(a',b'))

(***) :: StackWidening Stack a -> StackWidening Stack b -> StackWidening Stack (a,b)
(***) f g (a,b) = do
  Stack i st <- get
  let ((la,a'),Stack i' as') = runState (f a) (Stack i (map fst st))
      ((lb,b'),Stack i'' bs') = runState (g b) (Stack i' (map snd st))
  put (Stack i'' (zip as' bs'))
  return (la ⊔ lb,(a',b'))

topOut :: (Eq a,UpperBounded a) => StackWidening Stack a
topOut = topOut' top

topOut' :: Eq a => a -> StackWidening Stack a
topOut' t x = do
  Stack _ st <- get
  case st of
    []                -> return (NoLoop,x)
    (l:_) | l /= t    -> return (NoLoop,t)
          | otherwise -> return (Loop,t)


instance PreOrd Loop where
  NoLoop ⊑ NoLoop = True
  NoLoop ⊑ Loop = True
  Loop ⊑ Loop = True
  _ ⊑ _ = False

instance Complete Loop where
  NoLoop ⊔ l = l
  l ⊔ NoLoop = l
  Loop ⊔ Loop = Loop
