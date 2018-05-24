{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
module Control.Arrow.Utils where

import Control.Arrow

-- | Applies a computation to all elements of the input list and
-- collects the results in an list.
mapA :: ArrowChoice c => c x y -> c [x] [y]
mapA f = proc l -> case l of
  [] -> returnA -< []
  (a:as) -> do
    b <- f -< a
    bs <- mapA f -< as
    returnA -< (b:bs)

-- | Throws away the result of a computation.
voidA :: Arrow c => c x y -> c x ()
voidA f = proc x -> do
  _ <- f -< x
  returnA -< ()

infixr 1 &&>
(&&>) :: Arrow c => c a () -> c a b -> c a b
f &&> g = f &&& g >>> arr snd

-- | Projects the first component of a product.
pi1 :: Arrow c => c (x,y) x
pi1 = arr fst

-- | Projects the second component of a product.
pi2 :: Arrow c => c (x,y) y
pi2 = arr snd

-- | Zips two lists together.
zipWithA :: ArrowChoice c => c (x,y) z -> c ([x],[y]) [z]
zipWithA f = proc (l1,l2) -> case (l1,l2) of
  ([],_) -> returnA -< []
  (_,[]) -> returnA -< []
  (a:as,b:bs) -> do
    c <- f -< (a,b)
    cs <- zipWithA f -< (as,bs)
    returnA -< c:cs

-- | Folds a computation over a list from left to right.
foldA :: ArrowChoice c => c (a,x) a -> c ([x],a) a
foldA f = proc (l,a) -> case l of
  (x:xs) -> do
    a' <- f -< (a,x)
    foldA f -< (xs,a')
  [] -> returnA -< a

-- | Duplicates the current value.
duplicate :: Arrow c => c x (x,x)
duplicate = arr (\x -> (x,x))

-- | creates a computation that always returns the same value.
constA :: Arrow c => c () x -> c y x
constA f = arr (\_ -> ()) >>> f
