{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.GaloisConnection where

import           Data.Hashable
import           Data.Order
import qualified Data.Abstract.Powerset as Abs
import qualified Data.Concrete.Powerset as Con
import qualified Data.Concrete.Error as Con
import qualified Data.Abstract.Except as Abs
import qualified Data.Abstract.Failure as AbsF
import qualified Data.Concrete.Boolean as Con
import qualified Data.Abstract.Boolean as Abs
import qualified Data.Abstract.Interval as Abs
import           Data.Identifiable

-- | A galois connection consisting of an abstraction function alpha
-- and a concretization function gamma between two pre-ordered sets
-- has to satisfy forall x,y. alpha x ⊑ y iff x ⊑ gamma y
class (PreOrd x, PreOrd y) => Galois x y where
  alpha :: x -> y
  gamma :: y -> x

instance (Galois x x', Galois y y') => Galois (x,y) (x',y') where
  alpha (x,y) = (alpha x, alpha y)
  gamma (x',y') = (gamma x', gamma y')

instance (Identifiable (x,y), Galois (Con.Pow x) x', Galois (Con.Pow y) y')
  => Galois (Con.Pow (x,y)) (x',y') where
  alpha m = (alpha (fst <$> m),alpha (snd <$> m))
  gamma m = Con.cartesian (gamma (fst m),gamma (snd m))

instance Galois (Con.Pow Con.Bool) Abs.Bool where
  alpha = lifted $ \b -> case b of
    Con.True -> Abs.True
    Con.False -> Abs.False
  gamma x = case x of
    Abs.Top   -> [Con.True, Con.False]
    Abs.True  -> [Con.True]
    Abs.False -> [Con.False]

instance (Hashable a, Eq a, Ord a, Enum a) => Galois (Con.Pow a) (Abs.Interval a) where
  alpha x = Abs.Interval (minimum x) (maximum x)
  gamma (Abs.Interval x y) = [x..y]

instance (Complete a', Eq a, Hashable a, Galois (Con.Pow a) a', Eq b, Hashable b, Complete b', Galois (Con.Pow b) b')
    => Galois (Con.Pow (Con.Error a b)) (Abs.Except a' b') where
  alpha = lifted $ \e -> case e of
    Con.Fail x -> Abs.Fail (alphaSing x)
    Con.Success y -> Abs.Success (alphaSing y)
  gamma = error "noncomputable"

instance (Eq e, Hashable e, Eq b, Hashable b, Complete b', Galois (Con.Pow b) b')
    => Galois (Con.Pow (Con.Error e b)) (AbsF.Failure e b') where
  alpha = lifted $ \e -> case e of
    Con.Fail x -> AbsF.Fail x
    Con.Success y -> AbsF.Success (alphaSing y)
  gamma = error "noncomputable"

instance (Hashable a, Eq a, Complete a', Galois (Con.Pow a) a')
    => Galois (Con.Pow (Maybe a)) (Maybe a') where
  alpha = lifted $ \e -> case e of
    Just x -> Just (alphaSing x)
    Nothing -> Nothing
  gamma = error "noncomputable"

instance Galois (Con.Pow a) a' => Galois (Con.Pow a) (Abs.Pow a') where
  alpha x = Abs.fromFoldable (fmap alphaSing x)
  gamma y' = Con.unions (Con.fromFoldable (fmap gamma y'))

instance Galois (Con.Pow ()) () where
  alpha = const ()
  gamma = const $ Con.singleton ()

alphaSing :: Galois (Con.Pow x) x' => x -> x'
alphaSing = alpha . (return :: x -> Con.Pow x)

lifted :: Complete y => (x -> y) -> Con.Pow x -> y
lifted lift = foldr1 (⊔) . fmap lift
