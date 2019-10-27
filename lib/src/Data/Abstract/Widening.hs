module Data.Abstract.Widening where

import Data.Abstract.Stable
import Data.Order

-- | ▽ has to be an upper bound operator, i.e. x ⊑ x ▽ y and y ⊑ x ▽ y.
-- Furthermore, iterating ▽ on an ascending chain has to stabilize:
-- Let x1, x2, ... xn be an infinite ascending chain, then x1, x1 ▽
-- x2, (x1 ▽ x2) ▽ x3, ... (similar to left fold) has a limit:
--
--            ... finite
--          ▽
--        /   \
--       ▽     \
--     /   \    \
--    ▽     \    \
--  /   \    \    \
-- x1 ⊑ x2 ⊑ x3 ⊑ x4 ⊑ ... infinite
--
-- Furthermore, we allow our widening operators to maintain some state
-- between each iteration.
type Widening a = a -> a -> (Stable,a)

-- | For a preorder with no infinite ascending chains, (⊔) is a
-- trivial widening operator.
finite :: Complete a => Widening a
finite a b = let x = a ⊔ b in (if x ⊑ a then Stable else Unstable,x)
{-# INLINE finite #-}

toJoin :: (Widening a -> Widening b) -> (a -> a -> a) -> (b -> b -> b)
toJoin f g a a' = snd (f (\b b' -> (Unstable,g b b')) a a')
{-# INLINE toJoin #-}

toJoin2 :: (Widening a -> Widening b -> Widening c) -> (a -> a -> a) -> (b -> b -> b) -> (c -> c -> c)
toJoin2 f g h c c' = snd (f (\a a' -> (Unstable,g a a')) (\b b' -> (Unstable,h b b')) c c')
{-# INLINE toJoin2 #-}

-- | Widening operator that joins until the given limit is
-- reached. Then it calls the fallback widening operator.
bounded :: Complete a => a -> Widening a -> Widening a
bounded limit w a b
  | b ⊑ limit = let b' = a ⊔ b
                in (if b' ⊑ b then Stable else Unstable,b')
  | otherwise = a `w` b
{-# INLINE bounded #-}

(**) :: Widening a -> Widening b -> Widening (a,b)
(**) wa wb (a1,b1) (a2,b2) =
    let (s1,a') = wa a1 a2
        (s2,b') = wb b1 b2
    in (s1 ⊔ s2, (a',b'))
{-# INLINE (**) #-}
