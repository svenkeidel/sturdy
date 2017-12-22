{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.GaloisConnection where

import Data.Order
import Data.Powerset
import Data.Hashable

-- | A galois connection consisting of an abstraction function alpha
-- and a concretization function gamma between two pre-ordered sets
-- has to satisfy forall x,y. alpha x ⊑ y iff x ⊑ gamma y
class (PreOrd x, PreOrd y) => Galois x y where
  alpha :: x -> y
  gamma :: y -> x

instance PreOrd a => Galois a a where
  alpha = id
  gamma = id

instance (Galois x x', Galois y y') => Galois (x,y) (x',y') where
  alpha (x,y) = (alpha x, alpha y)
  gamma (x',y') = (gamma x', gamma y')

instance (Eq (x,y), Hashable (x,y), Galois (Pow x) x', Galois (Pow y) y')
  => Galois (Pow (x,y)) (x',y') where
  alpha m = (alpha (fst <$> m),alpha (snd <$> m))
  gamma m = cartesian (gamma (fst m),gamma (snd m))

alphaSing :: Galois (Pow x) x' => x -> x'
alphaSing = alpha . (return :: x -> Pow x)

lifted :: (Complete y, LowerBounded y) => (x -> y) -> Pow x -> y
lifted lift = foldr ((⊔) . lift) bottom