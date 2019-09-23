{-# LANGUAGE FlexibleInstances #-}
module Data.Metric where

type Metric a n = a -> a -> n

unit :: Metric n ()
unit _ _ = ()
{-# INLINE unit #-}

euclid :: Num n => Metric n n
euclid x y = abs (x - y)
{-# INLINE euclid #-}

discrete :: Eq n => Metric n Ordering
discrete x y
  | x == y = EQ
  | otherwise = GT
{-# INLINE discrete #-}

(**) :: Metric n1 m1 -> Metric n2 m2 -> Metric (n1,n2) (Product m1 m2)
(**) m1 m2 (x1,y1) (x2,y2) = Product (m1 x1 x2) (m2 y1 y2)
{-# INLINE (**) #-}

data Product m n = Product m n deriving (Eq,Ord)

instance (Num m, Num n) => Num (Product n m) where
  Product x1 y1 + Product x2 y2 = Product (x1 + x2) (y1 + y2)
  Product x1 y1 - Product x2 y2 = Product (x1 - x2) (y1 - y2)
  Product x1 y1 * Product x2 y2 = Product (x1 * x2) (y1 * y2)
  negate (Product x y) = Product (negate x) (negate y)
  abs (Product x y) = Product (abs x) (abs y)
  signum (Product x y) = Product (signum x) (signum y)
  fromInteger n = Product (fromInteger n) (fromInteger n)

instance (Show n, Show m) => Show (Product n m) where
  show (Product n m) = show (n,m)
