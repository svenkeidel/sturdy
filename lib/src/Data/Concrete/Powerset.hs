{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Concrete.Powerset where

import           Prelude hiding ((.),seq)

import           Control.Category
import           Control.Applicative
import           Control.Monad

import           Data.Sequence (Seq)
import qualified Data.Sequence as S
import           Data.Hashable
import           Data.HashSet (HashSet)
import qualified Data.HashSet as H
import           Data.Foldable (foldl')
import qualified Data.Foldable as F
import           Data.List (intercalate)
import           Data.Order
import           Data.Identifiable

import GHC.Generics (Generic)
import GHC.Exts

newtype Pow a = Pow (Seq a) deriving (Functor, Applicative, Monad, Alternative, MonadPlus, Semigroup, Monoid, Foldable, Traversable, Generic)

instance Identifiable a => PreOrd (Pow a) where
  as ⊑ bs = all (`H.member` toHashSet as) (toHashSet bs)

instance Identifiable a => LowerBounded (Pow a) where
  bottom = mempty

instance Identifiable a => Eq (Pow a) where
  as == bs = toHashSet as == toHashSet bs

instance Identifiable a => Complete (Pow a) where
  as ⊔ bs = as `union` bs

instance Show a => Show (Pow a) where
  show (Pow a) = "{" ++ intercalate ", " (show <$> toList a) ++ "}"

instance (Identifiable a) => Hashable (Pow a) where
  hashWithSalt salt x = hashWithSalt salt (toHashSet x)

instance IsList (Pow a) where
  type Item (Pow a) = a
  toList = F.toList
  fromList = Pow . S.fromList

empty :: Pow a
empty = mempty

singleton :: a -> Pow a
singleton = Pow . return

union :: Pow a -> Pow a -> Pow a
union = mappend

cartesian :: (Pow a, Pow b) -> Pow (a,b)
cartesian (as,bs) = do
  a <- as
  b <- bs
  return (a,b)

toHashSet :: (Identifiable a) => Pow a -> HashSet a
toHashSet = foldl' (flip H.insert) H.empty

fromFoldable :: (Foldable f, Monad t, Monoid (t a)) => f a -> t a
fromFoldable = foldMap return

size :: Foldable f => f a -> Int
size = length

dedup :: (Identifiable a) => Pow a -> Pow a
dedup = fromFoldable . toHashSet

powmap :: (a -> b) -> Pow a -> Pow b
powmap = fmap

unions :: Pow (Pow a) -> Pow a
unions = join
