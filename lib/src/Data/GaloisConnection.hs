{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.GaloisConnection where

import           Data.Hashable
import           Data.Order
import           Data.Concrete.Powerset
import qualified Data.Concrete.Error as Con
import qualified Data.Abstract.Error as Abs
import qualified Data.Concrete.Boolean as Con
import qualified Data.Abstract.Boolean as Abs
import qualified Data.Abstract.Interval as Abs
import           Control.Arrow

-- | A galois connection consisting of an abstraction function alpha
-- and a concretization function gamma between two pre-ordered sets
-- has to satisfy forall x,y. alpha x ⊑ y iff x ⊑ gamma y
class (PreOrd x, PreOrd y) => Galois x y where
  alpha :: x -> y
  gamma :: y -> x

instance (Galois x x', Galois y y') => Galois (x,y) (x',y') where
  alpha (x,y) = (alpha x, alpha y)
  gamma (x',y') = (gamma x', gamma y')

instance (Eq (x,y), Hashable (x,y), Galois (Pow x) x', Galois (Pow y) y')
  => Galois (Pow (x,y)) (x',y') where
  alpha m = (alpha (fst <$> m),alpha (snd <$> m))
  gamma m = cartesian (gamma (fst m),gamma (snd m))

instance Galois (Pow Con.Bool) Abs.Bool where
  alpha = lifted $ \b -> case b of
    Con.True -> Abs.True
    Con.False -> Abs.False
  gamma x = case x of
    Abs.Top   -> [Con.True, Con.False]
    Abs.True  -> [Con.True]
    Abs.False -> [Con.False]

instance (Hashable a, Eq a, Ord a, Enum a) => Galois (Pow a) (Abs.Interval a) where
  alpha x = Abs.Interval (minimum x) (maximum x)
  gamma (Abs.Interval x y) = [x..y]

instance (Galois (m y) (n y'), Galois x x') => Galois (Kleisli m x y) (Kleisli n x' y') where
  alpha (Kleisli f) = Kleisli (alpha . f . gamma)
  gamma (Kleisli f) = Kleisli (gamma . f . alpha)

instance (Eq a, Hashable a, Galois (Pow a) a', Eq b, Hashable b, Complete b', Galois (Pow b) b')
    => Galois (Pow (Con.Error a b)) (Abs.Error a' b') where
  alpha = lifted $ \e -> case e of
    Con.Fail x -> Abs.Fail (alphaSing x)
    Con.Success y -> Abs.Success (alphaSing y)
  gamma = error "noncomputable"

alphaSing :: Galois (Pow x) x' => x -> x'
alphaSing = alpha . (return :: x -> Pow x)

lifted :: Complete y => (x -> y) -> Pow x -> y
lifted lift = foldr1 (⊔) . fmap lift
