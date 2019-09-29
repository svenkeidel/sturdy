{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Abstract.IntersectionSet where

import           Data.Abstract.Widening
import           Data.Abstract.Stable
import           Data.HashSet (HashSet)
import qualified Data.HashSet as H
import           Data.Order
import           Data.Identifiable
import           Data.Hashable
import           Data.Empty
import           Data.Coerce

newtype Set a = Set (HashSet a) deriving (Eq, Hashable)

instance Show a => Show (Set a) where
  show (Set s) = "{" ++ init (tail (show (H.toList s))) ++ "}"

instance Identifiable a => PreOrd (Set a) where
  -- | xs ⊑ ys iff ys is a subset of xs
  Set xs ⊑ Set ys = all (`H.member` xs) ys
  {-# INLINE (⊑) #-}

instance Identifiable a => Complete (Set a) where
  Set xs ⊔ Set ys = Set (xs `H.intersection` ys)
  {-# INLINE (⊔) #-}

widening :: Identifiable a => Widening (Set a)
widening xs ys = let zs = xs ⊔ ys
                 in (if size xs == size zs && size ys == size zs then Stable else Unstable,zs)

instance IsEmpty (Set a) where
  empty = Set empty

fromList :: Identifiable a => [a] -> Set a
fromList = coerce H.fromList

member :: Identifiable a => a -> Set a -> Bool
member = coerce H.member

insert :: Identifiable a => a -> Set a -> Set a
insert = coerce H.insert

delete :: Identifiable a => a -> Set a -> Set a
delete = coerce H.delete

union :: Identifiable a => Set a -> Set a -> Set a
union = coerce H.union

intersection :: Identifiable a => Set a -> Set a -> Set a
intersection = coerce H.intersection

size :: Set a -> Int
size = coerce H.size
