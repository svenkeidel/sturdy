{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Monoidal where

class Monoidal m where
  mmap :: (a -> a') -> (b -> b') -> a `m` b -> a' `m` b'
  assoc1 :: (a `m` (b `m` c)) -> ((a `m` b) `m` c)
  assoc2 :: ((a `m` b) `m` c) -> (a `m` (b `m` c))

instance Monoidal (,) where
  mmap f g (x,y) = (f x,g y)
  assoc1 (a,(b,c)) = ((a,b),c)
  assoc2 ((a,b),c) = (a,(b,c))
  {-# INLINE mmap #-}
  {-# INLINE assoc1 #-}
  {-# INLINE assoc2 #-}

instance Monoidal Either where
  mmap f _ (Left x) = Left (f x)
  mmap _ g (Right y) = Right (g y)

  assoc1 (Left a) = Left (Left a)
  assoc1 (Right (Left b)) = Left (Right b)
  assoc1 (Right (Right c)) = Right c

  assoc2 (Left (Left a)) = Left a
  assoc2 (Left (Right b)) = Right (Left b)
  assoc2 (Right c) = Right (Right c)
  {-# INLINE mmap #-}
  {-# INLINE assoc1 #-}
  {-# INLINE assoc2 #-}

class Strong f m where
  strength1 :: f a `m` b -> f (a `m` b)
  strength2 :: a `m` f b -> f (a `m` b)

instance Functor f => Strong f (,) where
  strength1 (f,b) = fmap (\a -> (a,b)) f
  strength2 (a,f) = fmap (\b -> (a,b)) f
  {-# INLINE strength1 #-}
  {-# INLINE strength2 #-}

instance Applicative f => Strong f Either where
  strength1 (Left f) = fmap Left f
  strength1 (Right b) = pure (Right b)
  strength2 (Left a) = pure (Left a)
  strength2 (Right f) = fmap Right f
  {-# INLINE strength1 #-}
  {-# INLINE strength2 #-}

class Monoidal m => Symmetric m where
  commute :: a `m` b -> b `m` a

instance Symmetric (,) where
  commute (a,b) = (b,a)
  {-# INLINE commute #-}

instance Symmetric Either where
  commute (Left a) = Right a
  commute (Right a) = Left a
  {-# INLINE commute #-}

class Distributive m n where
  distribute1 :: a `m` (b `n` c) -> (a `m` b) `n` (a `m` c)
  distribute2 :: (a `m` b) `n` (a `m` c) -> a `m` (b `n` c)

instance Distributive (,) Either where
  distribute1 (a,x) = case x of
    Left b -> Left (a,b)
    Right c -> Right (a,c)
  {-# INLINE distribute1 #-}

  distribute2 (Left (a,b)) = (a,Left b)
  distribute2 (Right (a,c)) = (a,Right c)
  {-# INLINE distribute2 #-}

shuffle1 :: (a,(b,c)) -> (b,(a,c))
shuffle1 (a,(b,c)) = (b,(a,c))
{-# INLINE shuffle1 #-}

shuffle2 :: (b,(a,c)) -> (a,(b,c))
shuffle2 (b,(a,c)) = (a,(b,c))
{-# INLINE shuffle2 #-}

shuffle3 :: ((a,b),c) -> (b,(a,c))
shuffle3 ((a,b),c) = (b,(a,c))
{-# INLINE shuffle3 #-}
