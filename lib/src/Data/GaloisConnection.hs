{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.GaloisConnection where

import           Data.Hashable
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Order
import           Data.Powerset
import           Data.Error
import           Control.Arrow

-- | A galois connection consisting of an abstraction function alpha
-- and a concretization function gamma between two pre-ordered sets
-- has to satisfy forall x,y. alpha x ⊑ y iff x ⊑ gamma y
class (PreOrd x, PreOrd y) => Galois x y where
  alpha :: x -> y
  gamma :: y -> x

instance {-# OVERLAPS #-} (PreOrd a, PreOrd b, a ~ b) => Galois a b where
  alpha = id
  gamma = id

instance (Galois x x', Galois y y') => Galois (x,y) (x',y') where
  alpha (x,y) = (alpha x, alpha y)
  gamma (x',y') = (gamma x', gamma y')

instance (Eq (x,y), Hashable (x,y), Galois (Pow x) x', Galois (Pow y) y')
  => Galois (Pow (x,y)) (x',y') where
  alpha m = (alpha (fst <$> m),alpha (snd <$> m))
  gamma m = cartesian (gamma (fst m),gamma (snd m))

instance (Galois v1 v2, Ord k) => Galois (Map k v1) (Map k v2) where
  alpha = Map.map alpha
  gamma = Map.map gamma

instance (Galois (m y) (n y'), Galois x x') => Galois (Kleisli m x y) (Kleisli n x' y') where
  alpha (Kleisli f) = Kleisli (alpha . f . gamma)
  gamma (Kleisli f) = Kleisli (gamma . f . alpha)

instance (Eq a, Hashable a, Galois (Pow a) a', Eq b, Hashable b, Complete b', Galois (Pow b) b')
    => Galois (Pow (Either a b)) (Error a' b') where
  alpha = lifted $ \e -> case e of
    Left x -> Error (alphaSing x)
    Right y -> Success (alphaSing y)
  gamma = error "noncomputable"

-- instance (PreOrd b', Eq a',Hashable a',Galois x x', Galois y y') => Galois (Fix a b x y) (CacheArrow a' b' x' y') where
--   alpha f = liftCache (alpha . runFix f . gamma)
--   gamma f = liftFix (gamma . runCacheArrow f . alpha)

-- instance Galois (c x (Either e y)) (c' x' (Error e' y')) => Galois (EitherArrow e c x y) (ErrorArrow e' c' x' y') where
--   alpha (EitherArrow f) = ErrorArrow (alpha f)
--   gamma (ErrorArrow f) = EitherArrow (gamma f)

-- instance Galois (c x y) (c' x' y') => Galois (Environment var val c x y) (BoundedEnv var var val c' x' y') where
--   alpha (Environment )

alphaSing :: Galois (Pow x) x' => x -> x'
alphaSing = alpha . (return :: x -> Pow x)

lifted :: (Complete y, LowerBounded y) => (x -> y) -> Pow x -> y
lifted lift = foldr ((⊔) . lift) bottom
