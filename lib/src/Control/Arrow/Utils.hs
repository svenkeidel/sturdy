{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
module Control.Arrow.Utils where

import Prelude hiding (map,zipWith)
import Control.Arrow

-- | Applies a computation to all elements of the input list and
-- collects the results in an list.
map :: ArrowChoice c => c x y -> c [x] [y]
map f = proc l -> case l of
  [] -> returnA -< []
  (a:as) -> do
    b <- f -< a
    bs <- map f -< as
    returnA -< (b:bs)

-- | Throws away the result of a computation.
void :: Arrow c => c x y -> c x ()
void f = proc x -> do
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
zipWith :: ArrowChoice c => c (x,y) z -> c ([x],[y]) [z]
zipWith f = proc (l1,l2) -> case (l1,l2) of
  ([],_)      -> returnA -< []
  (_,[])      -> returnA -< []
  (a:as,b:bs) -> uncurry (:) ^<< f *** zipWith f -< ((a,b),(as,bs)) 

-- | Folds a computation over a list from left to right.
fold :: ArrowChoice c => c (a,x) a -> c ([x],a) a
fold f = proc (l,a) -> case l of
  (x:xs) -> do
    a' <- f -< (a,x)
    fold f -< (xs,a')
  [] -> returnA -< a

-- | Duplicates the current value.
duplicate :: Arrow c => c x (x,x)
duplicate = arr (\x -> (x,x))

-- | creates a computation that always returns the same value.
const :: Arrow c => c () x -> c y x
const f = arr (\_ -> ()) >>> f
