{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
module Data.Abstract.Singleton where

import           Data.Hashable
import           Data.Order

import           GHC.Generics (Generic)

data Singleton a = Single a | Any deriving (Eq, Functor, Foldable, Traversable, Generic)

instance Eq a => PreOrd (Singleton a) where
  _ ⊑ Any = True
  Single a ⊑ Single b = a == b
  _ ⊑ _ = False

instance (Hashable a) => Hashable (Singleton a)

instance Eq a => Complete (Singleton a) where
  Single a ⊔ Single b | a == b = Single a
  _ ⊔ _ = Any

instance Eq a => UpperBounded (Singleton a) where
  top = Any

instance Show a => Show (Singleton a) where
  show (Single a) = "Single " ++ show a
  show Any = "NonSingle"

allSingle :: [Singleton a] -> Maybe [a]
allSingle = mapM (\s -> case s of
  Single a -> Just a
  Any -> Nothing)
