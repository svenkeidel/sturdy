module Data.Empty where

class IsEmpty a where
  empty :: a

instance (IsEmpty a, IsEmpty b) => IsEmpty (a,b) where
  empty = (empty,empty)
