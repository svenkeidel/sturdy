{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Monoidal where

data Iso a b = Iso { to :: a -> b, from :: b -> a}

class Monoidal m where
  mmap :: (a -> a') -> (b -> b') -> a `m` b -> a' `m` b'
  assoc1 :: (a `m` (b `m` c)) -> ((a `m` b) `m` c)
  assoc2 :: ((a `m` b) `m` c) -> (a `m` (b `m` c))

instance Monoidal (,) where
  mmap f g ~(x,y) = (f x,g y)
  assoc1 ~(a,(b,c)) = ((a,b),c)
  assoc2 ~((a,b),c) = (a,(b,c))

instance Monoidal Either where
  mmap f _ (Left x) = Left (f x)
  mmap _ g (Right y) = Right (g y)

  assoc1 (Left a) = Left (Left a)
  assoc1 (Right (Left b)) = Left (Right b)
  assoc1 (Right (Right c)) = Right c

  assoc2 (Left (Left a)) = Left a
  assoc2 (Left (Right b)) = Right (Left b)
  assoc2 (Right c) = Right (Right c)

class Strong f m where
  strength1 :: f a `m` b -> f (a `m` b)
  strength2 :: a `m` f b -> f (a `m` b)

instance Functor f => Strong f (,) where
  strength1 (f,b) = fmap (\a -> (a,b)) f
  strength2 (a,f) = fmap (\b -> (a,b)) f

instance Applicative f => Strong f Either where
  strength1 (Left f) = fmap Left f
  strength1 (Right b) = pure (Right b)
  strength2 (Left a) = pure (Left a)
  strength2 (Right f) = fmap Right f

-- class Strong f m => StrongMonad f m where
--   mstrength :: f a `m` f b -> f (a `m` b)

-- instance Applicative f => StrongMonad f Either where 
--   mstrength (Left a) = fmap Left a
--   mstrength (Right b) = fmap Right b

class Monoidal m => Symmetric m where
  commute :: a `m` b -> b `m` a

instance Symmetric (,) where
  commute (a,b) = (b,a)

instance Symmetric Either where
  commute (Left a) = Right a
  commute (Right a) = Left a

class Distributive m n where
  distribute1 :: a `m` (b `n` c) -> (a `m` b) `n` (a `m` c)
  distribute2 :: (a `m` b) `n` (a `m` c) -> a `m` (b `n` c)

instance Distributive (,) Either where
  distribute1 (a,Left b) = Left (a,b)
  distribute1 (a,Right c) = Right (a,c)

  distribute2 (Left (a,b)) = (a,Left b)
  distribute2 (Right (a,c)) = (a,Right c)
