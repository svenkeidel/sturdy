{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveTraversable #-}
module Data.Powerset where

import           Prelude hiding (map,(.),id)

import           Control.Category
import           Control.Applicative
import           Control.Monad

import           Data.Sequence (Seq)
import           Data.Hashable
import           Data.HashSet (HashSet)
import qualified Data.HashSet as H
import           Data.Foldable (foldl',toList)
import           Data.List (intercalate)
import           Data.Order
 
newtype Pow a = Pow (Seq a) deriving (Eq, Functor, Applicative, Monad, Alternative, MonadPlus, Monoid, Foldable, Traversable)

instance (Eq a, Hashable a) => PreOrd (Pow a) where
  as ⊑ bs = all (`H.member` toHashSet as) (toHashSet bs)

instance (Eq a, Hashable a) => Complete (Pow a) where
  as ⊔ bs = as `union` bs

instance Show a => Show (Pow a) where
  show (Pow a) = "{" ++ intercalate ", " (show <$> toList a) ++ "}"

singleton :: Monad f => a -> f a
singleton = return

union :: (Monad f, Monoid (f a)) => f a -> f a -> f a
union = mappend

cartesian :: Monad f => (f a, f b) -> f (a,b)
cartesian (as,bs) = do
  a <- as
  b <- bs
  return (a,b)

toHashSet :: (Foldable f, Hashable a, Eq a) => f a -> HashSet a
toHashSet = foldl' (flip H.insert) H.empty

fromFoldable :: (Foldable f, Monad t, Monoid (t a)) => f a -> t a
fromFoldable = foldMap return

size :: Foldable f => f a -> Int
size = length
       
dedup :: (Hashable a, Eq a) => Pow a -> Pow a
dedup = fromFoldable . toHashSet
