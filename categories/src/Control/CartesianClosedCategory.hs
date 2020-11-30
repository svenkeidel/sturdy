{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ConstraintKinds #-}
module Control.CartesianClosedCategory
  ( toCategory
  , module Control.Category
  , Cartesian(..)
  , Cocartesian(..)
  , Distributive(..)
  , Closed(..)
  , Pointed(..)
  ) where

import Prelude hiding (id,(.))

import Control.Category
import GHC.Exts

toCategory
  :: forall (prim :: (* -> * -> *) -> Constraint) c x y.
     (prim c, Category c, Cartesian c, Cocartesian c, Closed c, Distributive c, Pointed c)
  => (x -> y) -> c x y
toCategory _ = error "toCategory"
{-# NOINLINE toCategory #-}

class Category c => Cartesian c where
  (&&&) :: c x y -> c x z -> c x (y,z)
  pi1   :: c (x,y) x
  pi2   :: c (x,y) y

class Category c => Cocartesian c where
  (+++) :: c x z -> c y z -> c (Either x y) z
  in1   :: c x (Either x y)
  in2   :: c y (Either x y)

class (Cartesian c, Cocartesian c) => Distributive c where
  distribute1 :: c (x, Either y z) (Either (x, y) (x, z))
  distribute2 :: c (Either (x, y) (x, z)) (x, Either y z)

class Category c => Closed c where
  apply :: c (x -> y, x) y
  curry :: c (x,y) z -> c x (y -> z)
  uncurry :: c x (y -> z) -> c (x,y) z

class Category c => Pointed c where
  const :: forall x p. p -> c x p

instance Cartesian (->) where
  f &&& g = \x -> (f x, g x)
  pi1 = \(x,_) -> x
  pi2 = \(_,y) -> y
  {-# INLINE (&&&) #-}
  {-# INLINE pi1 #-}
  {-# INLINE pi2 #-}

instance Cocartesian (->) where
  f +++ g = \case
    Left x -> f x
    Right y -> g y
  in1 = Left
  in2 = Right
  {-# INLINE (+++) #-}
  {-# INLINE in1 #-}
  {-# INLINE in2 #-}

instance Distributive (->) where
  distribute1 = \case
    (x, Left y)  -> Left (x,y)
    (x, Right z) -> Right (x,z)
  distribute2 = \case
    Left (x,y)  -> (x, Left y)
    Right (x,z) -> (x, Right z)
  {-# INLINE distribute1 #-}
  {-# INLINE distribute2 #-}

instance Closed (->) where
  apply = \(f,x) -> f x
  curry = \f x y -> f (x,y)
  uncurry = \f (x,y) -> f x y
  {-# INLINE apply #-}
  {-# INLINE curry #-}
  {-# INLINE uncurry #-}

instance Pointed (->) where
  const p _ = p
