{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
module Utils where

import Control.Arrow

eqLength :: [a] -> [b] -> Bool
eqLength [] [] = True
eqLength (_:as) (_:bs) = eqLength as bs
eqLength _ _ = False

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
