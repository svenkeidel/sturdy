{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Data.Numeric where

class Num n => Numeric n f | n -> f where
  (/) :: n -> n -> f n
  infixl 7 /
