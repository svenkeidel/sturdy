{-# LANGUAGE Arrows #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
module Data.Abstract.Powerset where

import           Prelude hiding ((.))

import           Control.Arrow hiding (ArrowMonad)
import           Control.Arrow.Monad
import           Control.Applicative hiding (empty)
import           Control.Category
import           Control.Monad

import           Data.Profunctor
import           Data.Sequence (Seq,(<|),viewl,ViewL(..))
import           Data.Hashable
import           Data.HashSet (HashSet)
import qualified Data.HashSet as H
import           Data.Foldable (foldl',toList)
import           Data.List (intercalate)
import           Data.Order

import           GHC.Generics (Generic)

newtype Pow a = Pow (Seq a) deriving (Functor, Applicative, Monad, Alternative, MonadPlus, Semigroup, Monoid, Foldable, Traversable, Generic)

instance PreOrd a => PreOrd (Pow a) where
  as ⊑ bs = all (\x -> any (x ⊑) bs) as

instance (Eq a, Hashable a) => Eq (Pow a) where
  as == bs = toHashSet as == toHashSet bs

instance PreOrd a => Complete (Pow a) where
  as ⊔ bs = as `union` bs

instance PreOrd a => LowerBounded (Pow a) where
  bottom = empty

instance UpperBounded a => UpperBounded (Pow a) where
  top = singleton top

instance Show a => Show (Pow a) where
  show (Pow a) = "{" ++ intercalate ", " (show <$> toList a) ++ "}"

instance (Eq a, Hashable a) => Hashable (Pow a) where
  hashWithSalt salt x = hashWithSalt salt (toHashSet x)

instance (ArrowChoice c, Profunctor c) => ArrowFunctor Pow c where
  mapA f = lmap toEither (arr (\_ -> empty) ||| rmap (\(y,Pow ys) -> Pow (y <| ys)) (f *** mapA f))

instance (ArrowChoice c, Profunctor c) => ArrowMonad Pow c where
  mapJoinA f = lmap toEither (arr (\_ -> empty) ||| rmap (\(ys,ys') -> ys <> join ys') (f *** mapA f))

toEither :: Pow a -> Either () (a,Pow a)
toEither (Pow s) = case viewl s of
  EmptyL -> Left ()
  x :< xs -> Right (x,Pow xs)

empty :: Pow a
empty = mempty

singleton :: a -> Pow a
singleton = Pow . return

insert :: a -> Pow a -> Pow a
insert a (Pow as) = Pow (a <| as)

union :: Pow a -> Pow a -> Pow a
union = mappend

toHashSet :: (Hashable a, Eq a) => Pow a -> HashSet a
toHashSet = foldl' (flip H.insert) H.empty

fromFoldable :: (Foldable f, Monad t, Monoid (t a)) => f a -> t a
fromFoldable = foldMap return

size :: Foldable f => f a -> Int
size = length

dedup :: (Hashable a, Eq a) => Pow a -> Pow a
dedup = fromFoldable . toHashSet
