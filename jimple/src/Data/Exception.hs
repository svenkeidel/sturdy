{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Exception where

import Data.GaloisConnection
import Data.Hashable
import Data.Order
import Data.String

import Data.Concrete.Powerset as Con

data Exception v = StaticException String | DynamicException v deriving (Eq)

instance Show v => Show (Exception v) where
  show (StaticException s) = "Static: " ++ s
  show (DynamicException v) = "Dynamic: " ++ show v

instance Hashable v => Hashable (Exception v) where
  hashWithSalt h (StaticException s) = h + hash s
  hashWithSalt h (DynamicException v) = h + hash v

instance PreOrd v => PreOrd (Exception v) where
  StaticException s1 ⊑ StaticException s2 = s1 == s2
  _ ⊑ StaticException _ = True
  DynamicException v1 ⊑ DynamicException v2 = v1 ⊑ v2
  _ ⊑ _ = False

instance LowerBounded v => LowerBounded (Exception v) where
  bottom = DynamicException bottom

instance Complete v => Complete (Exception v) where
  StaticException s1 ⊔ StaticException s2 = StaticException $ s1 ++ "\n" ++ s2
  StaticException s ⊔ _ = StaticException s
  _ ⊔ StaticException s = StaticException s
  DynamicException v1 ⊔ DynamicException v2 = DynamicException $ v1 ⊔ v2

instance IsString (Exception v) where
  fromString = StaticException

instance (Eq v, Hashable v, Complete v', Galois (Con.Pow v) v')
    => Galois (Con.Pow (Exception v)) (Exception v') where
  alpha = lifted $ \e -> case e of
    StaticException s -> StaticException s
    DynamicException v -> DynamicException $ alphaSing v
  gamma = error "noncomputable"
