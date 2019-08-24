{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Measure where

import Data.Order

class (Complete a, Num n, Ord n) => Measurable a n | a -> n where
  measure :: a -> n

instance (Measurable a1 n, Measurable a2 n) => Measurable (a1,a2) n where
  measure (a1,a2) = measure a1 * measure a2
