{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Measure where

import Data.Order
import Text.Printf

class (Complete a, Num n, Ord n) => Measurable a n | a -> n where
  measure :: a -> n

instance (Measurable a1 n, Measurable a2 n) => Measurable (a1,a2) n where
  measure (a1,a2) = measure a1 * measure a2
  {-# INLINE measure #-}

data Measured a n = Measured { argument :: a, measured :: n }

instance (Show a, Show n) => Show (Measured a n) where
  show m = printf "%s@%s" (show (argument m)) (show (measured m))

instance Ord n => Semigroup (Measured a n) where
  m1 <> m2
    | measured m1 <= measured m2 = m1
    | otherwise                  = m2
  {-# INLINE (<>) #-}
