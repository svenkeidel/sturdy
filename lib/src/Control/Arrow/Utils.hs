{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
module Control.Arrow.Utils where

import Prelude hiding (map,zipWith)
import Control.Arrow
import Data.Profunctor

-- | Applies a computation to all elements of the input list and
-- collects the results in an list.
map :: ArrowChoice c => c x y -> c [x] [y]
map f = proc l -> case l of
  [] -> returnA -< []
  (a:as) -> do
    b <- f -< a
    bs <- map f -< as
    returnA -< (b:bs)
{-# INLINABLE map #-}

when :: ArrowChoice c => c x () -> c (Bool,x) ()
when f = proc (b,x) ->
  if b
     then f -< x
     else returnA -< ()
{-# INLINE when #-}

-- | Throws away the result of a computation.
void :: Profunctor c => c x y -> c x ()
void f = rmap (\_ -> ()) f
{-# INLINE void #-}

infixr 1 &&>
(&&>) :: (Arrow c, Profunctor c) => c a () -> c a b -> c a b
f &&> g = rmap snd (f &&& g)
{-# INLINE (&&>) #-}

-- | Projects the first component of a product.
pi1 :: Arrow c => c (x,y) x
pi1 = arr fst
{-# INLINE pi1 #-}

-- | Projects the second component of a product.
pi2 :: Arrow c => c (x,y) y
pi2 = arr snd
{-# INLINE pi2 #-}

-- | Zips two lists together.
zipWith :: (ArrowChoice c,Profunctor c) => c (x,y) z -> c ([x],[y]) [z]
zipWith f = proc (l1,l2) -> case (l1,l2) of
  ([],_)      -> returnA -< []
  (_,[])      -> returnA -< []
  (a:as,b:bs) -> rmap (uncurry (:)) (f *** zipWith f) -< ((a,b),(as,bs)) 
{-# INLINABLE zipWith #-}

-- | Folds a computation over a list from left to right.
fold :: ArrowChoice c => c (a,x) a -> c ([x],a) a
fold f = proc (l,a) -> case l of
  (x:xs) -> do
    a' <- f -< (a,x)
    fold f -< (xs,a')
  [] -> returnA -< a
{-# INLINABLE fold #-}

-- | Duplicates the current value.
duplicate :: Arrow c => c x (x,x)
duplicate = arr (\x -> (x,x))
{-# INLINE duplicate #-}

-- | creates a computation that always returns the same value.
const :: Profunctor c => c () x -> c y x
const = lmap (\_ -> ())
{-# INLINE const #-}

all :: (ArrowChoice c, Profunctor c) => c a Bool -> c [a] Bool
all f = rmap and (map f)
{-# INLINE all #-}

any :: (ArrowChoice c, Profunctor c) => c a Bool -> c [a] Bool
any f = rmap or (map f)
{-# INLINE any #-}
