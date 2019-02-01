{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Monoidal where

data Iso a b = Iso { to :: a -> b, from :: b -> a}

class Monoidal m where
  mmap :: (a -> a') -> (b -> b') -> a `m` b -> a' `m` b'
  assoc :: Iso (a `m` (b `m` c)) ((a `m` b) `m` c)

instance Monoidal (,) where
  mmap f g (x,y) = (f x,g y)
  assoc = Iso (\(a,(b,c)) -> ((a,b),c)) (\((a,b),c) -> (a,(b,c)))

instance Monoidal Either where
  mmap f _ (Left x) = Left (f x)
  mmap _ g (Right y) = Right (g y)

  assoc = Iso assocTo assocFrom
    where
      assocTo :: Either a (Either b c) -> Either (Either a b) c
      assocTo (Left a) = Left (Left a)
      assocTo (Right (Left b)) = Left (Right b)
      assocTo (Right (Right c)) = Right c

      assocFrom :: Either (Either a b) c -> Either a (Either b c)
      assocFrom (Left (Left a)) = Left a
      assocFrom (Left (Right b)) = Right (Left b)
      assocFrom (Right c) = Right (Right c)

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

class Strong f m => StrongMonad f m where
  mstrength :: f a `m` f b -> f (a `m` b)

instance Applicative f => StrongMonad f Either where 
  mstrength (Left a) = fmap Left a
  mstrength (Right b) = fmap Right b

class Monoidal m => Symmetric m where
  commute :: a `m` b -> b `m` a

instance Symmetric (,) where
  commute (a,b) = (b,a)

instance Symmetric Either where
  commute (Left a) = Right a
  commute (Right a) = Left a

class Distributive m n where
  distribute :: Iso (a `m` (b `n` c)) ((a `m` b) `n` (a `m` c))

instance Distributive (,) Either where
  distribute = Iso distTo distFrom
    where
    distTo :: (a,Either b c) -> Either (a,b) (a,c)
    distTo (a,Left b) = Left (a,b)
    distTo (a,Right c) = Right (a,c)

    distFrom :: Either (a,b) (a,c) -> (a,Either b c)
    distFrom (Left (a,b)) = (a,Left b)
    distFrom (Right (a,c)) = (a,Right c)
