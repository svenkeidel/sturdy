{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
module Control.Arrow.Utils where

import Control.Arrow

import Data.Monoid
import Data.Order

mapA :: ArrowChoice c => c x y -> c [x] [y]
mapA f = proc l -> case l of
  [] -> returnA -< []
  (a:as) -> do
    b <- f -< a
    bs <- mapA f -< as
    returnA -< (b:bs)

mapA' :: ArrowChoice c => c (x,s) y -> c ([x],s) [y]
mapA' f = proc (l,s) -> case l of
  [] -> returnA -< []
  (a:as) -> do
    b <- f -< (a,s)
    bs <- mapA' f -< (as,s)
    returnA -< (b:bs)

foldA' :: ArrowChoice c => c (x,s) s -> c ([x],s) s
foldA' f = proc (xs,s) -> case xs of
  [] -> returnA -< s
  x:rest -> do
    s' <- f -< (x,s)
    foldA' f -< (rest,s')

voidA :: Arrow c => c x y -> c x ()
voidA f = proc x -> do
  _ <- f -< x
  returnA -< ()

infixr 1 &&>
(&&>) :: Arrow c => c a () -> c a b -> c a b
f &&> g = f &&& g >>> arr snd

pi1 :: Arrow c => c (x,y) x
pi1 = arr fst

pi2 :: Arrow c => c (x,y) y
pi2 = arr snd

zipWithA :: ArrowChoice c => c (x,y) z -> c ([x],[y]) [z]
zipWithA f = proc (l1,l2) -> case (l1,l2) of
  ([],_) -> returnA -< []
  (_,[]) -> returnA -< []
  (a:as,b:bs) -> do
    c <- f -< (a,b)
    cs <- zipWithA f -< (as,bs)
    returnA -< c:cs

foldA :: ArrowChoice c => c (a,x) a -> c ([x],a) a
foldA f = proc (l,a) -> case l of
  (x:xs) -> do
    a' <- f -< (a,x)
    foldA f -< (xs,a')
  [] -> returnA -< a

duplicate :: Arrow c => c x (x,x)
duplicate = arr (\x -> (x,x))

constA :: Arrow c => c () x -> c y x
constA f = arr (\_ -> ()) >>> f
