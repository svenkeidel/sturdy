{-# LANGUAGE Arrows #-}
module Control.Arrow.Utils where

import Control.Arrow

mapA :: ArrowChoice c => c x y -> c [x] [y]
mapA f = proc l -> case l of
  [] -> returnA -< []
  (a:as) -> do
    b <- f -< a
    bs <- mapA f -< as
    returnA -< (b:bs)

pi1 :: Arrow c => c (x,y) x
pi1 = arr fst

pi2 :: Arrow c => c (x,y) y
pi2 = arr snd
