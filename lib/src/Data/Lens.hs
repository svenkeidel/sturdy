{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
module Data.Lens where

import Data.Profunctor
import Data.Functor.Const
import Data.Functor.Identity
import Data.Monoid

{- 
Haskell code for the paper "Profunctor Optics: Modular Data Accessors"
by Matthew Pickering, Jeremy Gibbons, and Nicolas Wu

to appear in "The Art, Science, and Engineering of Programming" 1(2), 2017
 -}
type Optic p s t a b = p a b -> p s t

type Iso s t a b = forall p. Profunctor p => Optic p s t a b
iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso = dimap
{-# INLINE iso #-}

type Iso' a b = Iso a a b b
iso' :: (a -> b) -> (b -> a) -> Iso' a b
iso' = iso
{-# INLINE iso' #-}

type Lens s t a b = forall p. Strong p => Optic p s t a b
lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens f g = dimap (\s -> (f s,s)) (\(b,s) -> g s b) . first'
{-# INLINE lens #-}

_1 :: Lens (a,c) (b,c) a b
_1 = lens fst (\(_,c) b -> (b,c))
{-# INLINE _1 #-}

_2 :: Lens (c,a) (c,b) a b
_2 = lens snd (\(c,_) b -> (c,b))
{-# INLINE _2 #-}

type Lens' a b = Lens a a b b
lens' :: (a -> b) -> (a -> b -> a) -> Lens' a b
lens' = lens
{-# INLINE lens' #-}

type Prism s t a b = forall p. Choice p => Optic p s t a b
prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism f g = dimap g (either id f) . right'
{-# INLINE prism #-}

type Prism' a b = Prism a a b b
prism' :: (b -> a) -> (a -> Maybe b) -> Prism' a b
prism' f g = dimap (\a -> maybe (Left a) Right (g a)) (either id f) . right'
{-# INLINE prism' #-}

head :: Prism' [a] (a,[a])
head = prism' (\(a,as) -> a:as) (\xs -> case xs of (a:as) -> Just (a,as); _ -> Nothing)
{-# INLINE head #-}

type Getter r s t a b = Optic (Star (Const r)) s t a b
get :: Getter a s t a b -> s -> a
get f = getConst . runStar (f (Star Const))
{-# INLINE get #-}

getMaybe :: Getter (First a) s t a b -> s -> Maybe a
getMaybe f = getFirst . getConst . runStar (f (Star $ Const . First . Just))
{-# INLINE getMaybe #-}

type Setter s t a b = Optic (Star Identity) s t a b
set :: Setter s t a b -> b -> s -> t
set f b = runIdentity . runStar (f (Star (\_ -> Identity b)))
{-# INLINE set #-}

second :: Optic (SecondT e p) s t a b -> Optic p (e,s) (e,t) (e,a) (e,b)
second f g = runSecondT (f (SecondT g))
{-# INLINE second #-}

first :: Optic (FirstT e p) s t a b -> Optic p (s,e) (t,e) (a,e) (b,e)
first f g = runFirstT (f (FirstT g))
{-# INLINE first #-}

newtype FirstT u c x y = FirstT { runFirstT :: c (x,u) (y,u) }

instance Profunctor c => Profunctor (FirstT u c) where
  dimap f g (FirstT h) = FirstT (dimap (first' f) (first' g) h)
  lmap f (FirstT h) = FirstT (lmap (first' f) h)
  rmap f (FirstT h) = FirstT (rmap (first' f) h)
  {-# INLINE dimap #-}
  {-# INLINE lmap #-}
  {-# INLINE rmap #-}

instance (Strong c) => Strong (FirstT u c) where
  first' (FirstT f) = FirstT (dimap (\((a,c),u) -> ((a,u),c)) (\((a,u),c) -> ((a,c),u)) (first' f))
  second' (FirstT f) = FirstT (dimap (\((a,c),u) -> (a,(c,u))) (\(a,(c,u)) -> ((a,c),u)) (second' f))
  {-# INLINE first' #-}
  {-# INLINE second' #-}

instance (Choice c) => Choice (FirstT u c) where
  left' (FirstT f) = FirstT (dimap (\(e,u) -> either (Left . (,u)) (Right . (,u)) e) (either (first' Left) (first' Right)) (left' f))
  right' (FirstT f) = FirstT (dimap (\(e,u) -> either (Left . (,u)) (Right . (,u)) e) (either (first' Left) (first' Right)) (right' f))
  {-# INLINE left' #-}
  {-# INLINE right' #-}

newtype SecondT u c x y = SecondT { runSecondT :: c (u,x) (u,y) }

instance Profunctor c => Profunctor (SecondT u c) where
  dimap f g (SecondT h) = SecondT (dimap (second' f) (second' g) h)
  lmap f (SecondT h) = SecondT (lmap (second' f) h)
  rmap f (SecondT h) = SecondT (rmap (second' f) h)
  {-# INLINE dimap #-}
  {-# INLINE lmap #-}
  {-# INLINE rmap #-}

instance (Strong c) => Strong (SecondT u c) where
  first' (SecondT f) = SecondT (dimap (\(a,(c,u)) -> ((a,c),u)) (\((a,c),u) -> (a,(c,u))) (first' f))
  second' (SecondT f) = SecondT (dimap (\(u,(c,a)) -> (c,(u,a))) (\(c,(u,a)) -> (u,(c,a))) (second' f))
  {-# INLINE first' #-}
  {-# INLINE second' #-}

instance (Choice c) => Choice (SecondT u c) where
  left' (SecondT f) = SecondT (dimap (\(u,e) -> either (Left . (u,)) (Right . (u,)) e) (either (second' Left) (second' Right)) (left' f))
  right' (SecondT f) = SecondT (dimap (\(u,e) -> either (Left . (u,)) (Right . (u,)) e) (either (second' Left) (second' Right)) (right' f))
  {-# INLINE left' #-}
  {-# INLINE right' #-}

-- | Due to https://hackage.haskell.org/package/lens-4.17/docs/src/Control.Lens.Internal.Iso.html
data Exchange a b s t = Exchange (s -> a) (b -> t)

instance Profunctor (Exchange a b) where
  dimap f g (Exchange f' g') = Exchange (f' . f) (g . g')
  lmap f (Exchange f' g') = Exchange (f' . f) g'
  rmap g (Exchange f' g') = Exchange f' (g . g')
  {-# INLINE dimap #-}
  {-# INLINE lmap #-}
  {-# INLINE rmap #-}

withIso :: Iso s t a b -> ((s -> a) -> (b -> t) -> r) -> r
withIso f k = let (Exchange f' g') = f (Exchange id id)
              in k f' g'
{-# INLINE withIso #-}

from :: Iso s t a b -> Iso b a t s
from f = withIso f $ \f' g' -> iso g' f'
{-# INLINE from #-}
