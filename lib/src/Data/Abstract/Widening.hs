module Data.Abstract.Widening where

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
type Widening a = a -> a -> a

-- | For a preorder with no infinite ascending chains, (⊔) is a
-- trivial widening operator.
finite :: Complete a => Widening a
finite a b = a ⊔ b

-- | Widening operator that joins until the given limit is
-- reached. Then it calls the fallback widening operator.
bounded :: Complete a => a -> Widening a -> Widening a
bounded limit w a b
  | b ⊑ limit = a ⊔ b
  | otherwise = a `w` b

function :: Widening b -> Widening (a -> b) 
function w f g = \x -> (f x) `w` (g x)

(**) :: Widening a -> Widening b -> Widening (a,b)
(**) wa wb (a1,b1) (a2,b2) = (wa a1 a2, wb b1 b2)
