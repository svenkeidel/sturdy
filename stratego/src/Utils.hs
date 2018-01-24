{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
module Utils where

import Control.Arrow

eqLength :: [a] -> [b] -> Bool
eqLength [] [] = True
eqLength (_:as) (_:bs) = eqLength as bs
eqLength _ _ = False

mapA :: (ArrowChoice p) => p a b -> p [a] [b]
mapA f = proc l -> case l of
  (t:ts) -> do
    t' <- f -< t
    ts' <- mapA f -< ts
    returnA -< (t':ts')
  [] -> returnA -< []

zipWithA :: ArrowChoice p => p (a,b) c -> p ([a],[b]) [c]
zipWithA f = proc x -> case x of
  (a:as,b:bs) -> do
    c <- f -< (a,b)
    cs <- zipWithA f -< (as,bs)
    returnA -< c:cs
  _ -> returnA -< []

permutations :: [[s]] -> [[s]]
permutations l = case l of
  [] -> [[]]
  (xs:rs) -> do
    x <- xs
    ys <- permutations rs
    return (x:ys)

(~<<) :: Arrow c => (x -> y -> z) -> c u (x,y) -> c u z
f ~<< g = arr (uncurry f) <<< g
infixr 1 ~<<

(<<~) :: Arrow c => c z u -> (x -> y -> z) -> c (x,y) u
f <<~ g = f <<< arr (uncurry g)
infixr 1 <<~

unless :: (ArrowChoice p, ArrowZero p) => p Bool ()
unless = proc b -> if b then returnA -< () else zeroArrow -< ()

allA :: ArrowChoice p => p a Bool -> p [a] Bool
allA f = arr and <<< mapA f

anyA :: ArrowChoice p => p a Bool -> p [a] Bool
anyA f = arr or <<< mapA f

pi1 :: Arrow c => c (x,y) x
pi1 = arr fst

pi2 :: Arrow c => c (x,y) y
pi2 = arr snd
