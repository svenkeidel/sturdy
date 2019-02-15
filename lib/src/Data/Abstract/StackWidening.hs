{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
module Data.Abstract.StackWidening where

import           Prelude hiding (pred)

import           Data.Order
import           Data.HashMap.Lazy(HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.Identifiable
import           Data.Maybe
import           Data.Abstract.Widening(Widening,Stable(..))
import           Data.Functor.Const
import           Data.Lens

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
type StackWidening s a = s a -> a -> (s a,(Loop,a))

-- | Datatype that signals that we are in a loop.
data Loop = Loop | NoLoop deriving (Show,Eq)

data Unit a = Unit deriving (Show)
instance Semigroup (Unit a) where (<>) = mappend
instance Monoid (Unit a) where
  mempty = Unit
  mappend _ _ = Unit

-- | Trivial stack widening if the abstract domain is finite.
finite :: StackWidening Unit a
finite u a = (u,(NoLoop,a))

finite' :: StackWidening s a
finite' u a = (u,(NoLoop,a))

filter :: (a -> Bool) -> StackWidening s a -> StackWidening s a
filter pred widen s a
  | pred a = widen s a
  | otherwise = (s,(NoLoop,a))

filter' :: Prism' a b -> StackWidening s b -> StackWidening (Const (s b)) a
filter' pred widen s a = case getMaybe pred a of
  Just b ->
    let (s',(l,b')) = widen (getConst s) b
    in (Const s',(l,set pred b' a))
  Nothing -> (s,(NoLoop,a))

project :: Lens' a b -> StackWidening s b -> StackWidening (Const (s b)) a
project f widen s a =
  let (s',(l,b)) = widen (getConst s) (get f a)
  in (Const s',(l,set f b a))

data Stack a = Stack Int [a]
instance Semigroup (Stack a) where (<>) = mappend
instance Monoid (Stack a) where
  mempty = Stack 0 []
  mappend (Stack n st) (Stack n' st') = Stack (n+n') (st ++ st')
instance Show a => Show (Stack a) where show (Stack _ xs) = show xs

-- | Pushes elements onto a stack and increases its size. Always calls
-- the given widening.
stack :: StackWidening Stack a -> StackWidening Stack a
stack f s@(Stack n st) x =
  let (_,(l,x')) = f s x
  in (Stack (n+1) (x':st),(l,x'))

-- | Return the same elements until the specified maximum stack size
-- is reached, then call the fallback widening.
maxSize :: Int -> StackWidening Stack a -> StackWidening Stack a
maxSize limit fallback s@(Stack n _) x
  | n < limit = (s,(NoLoop,x))
  | otherwise = fallback s x

-- | Reuse an element from the stack that is greater than the current
-- element. If no such elements exist, call the fallback widening.
reuse :: PreOrd a => (a -> [a] -> a) -> StackWidening Stack a -> StackWidening Stack a
reuse bestChoice fallback s@(Stack _ st) x
  | null st               = (s,(NoLoop,x))
  | not (null candidates) = (s,(Loop,bestChoice x candidates))
  | otherwise             = fallback s x
  where
    -- All elements in the stack that are greater than the current
    -- element are potential candidates.
    candidates = [ y | y <- st, x ⊑ y ]

reuseFirst :: PreOrd a => StackWidening Stack a -> StackWidening Stack a
reuseFirst = reuse (\_ -> head)

data Groups k s a = Groups (HashMap k (s a))
instance (Identifiable k, Monoid (s a)) => Semigroup (Groups k s a) where (<>) = mappend
instance (Identifiable k, Monoid (s a)) => Monoid (Groups k s a) where
  mempty = Groups M.empty
  mappend (Groups m) (Groups m') = Groups (m `mappend` m')
instance (Show k, Show (s a)) => Show (Groups k s a) where
  show (Groups m) = show (M.toList m)
                    
groupBy :: (Monoid (s a), Identifiable k) => (a -> k) -> StackWidening s a -> StackWidening (Groups k s) a
groupBy f fallback (Groups m) a =
  let k = f a
      s = fromMaybe mempty (M.lookup k m)
      (s',(l,a')) = fallback s a
  in (Groups (M.insert k s' m), (l,a'))

fromWidening :: Complete a => Widening a -> StackWidening Stack a
fromWidening w s@(Stack _ l) a = case l of
  []  -> (s,(NoLoop,a))
  x:_ -> let (stable,x') = x `w` (x ⊔ a)
         in (s,(case stable of Stable -> Loop; Instable -> NoLoop,x'))

data Product s1 s2 x where
  Product :: s1 a -> s2 b -> Product s1 s2 (a,b)
instance (Monoid (s1 a), Monoid (s2 b)) => Semigroup (Product s1 s2 (a,b)) where (<>) = mappend
instance (Monoid (s1 a), Monoid (s2 b)) => Monoid (Product s1 s2 (a,b)) where
  mempty = Product mempty mempty
  mappend (Product s1 s2) (Product s1' s2') = Product (s1 <> s1') (s2 <> s2')
instance (Show (s1 a), Show (s2 b)) => Show (Product s1 s2 (a,b)) where
  show (Product s1 s2) = show (s1,s2)

(**) :: StackWidening s1 a -> StackWidening s2 b -> StackWidening (Product s1 s2) (a,b)
(**) f g (Product s1 s2) (a,b) =
  let (s1',(la,a')) = f s1 a
      (s2',(lb,b')) = g s2 b
  in (Product s1' s2',(la ⊔ lb,(a',b')))

topOut :: (Eq a,UpperBounded a) => StackWidening Stack a
topOut = topOut' top

topOut' :: Eq a => a -> StackWidening Stack a
topOut' t = topOut'' (const t)

topOut'' :: Eq a => (a -> a) -> StackWidening Stack a
topOut'' f s@(Stack _ st) x = case st of
  []                -> (s,(NoLoop,x))
  (l:_) | l /= f x  -> (s,(NoLoop,f x))
        | otherwise -> (s,(Loop,f x))

instance PreOrd Loop where
  NoLoop ⊑ NoLoop = True
  NoLoop ⊑ Loop = True
  Loop ⊑ Loop = True
  _ ⊑ _ = False

instance Complete Loop where
  NoLoop ⊔ l = l
  l ⊔ NoLoop = l
  Loop ⊔ Loop = Loop
