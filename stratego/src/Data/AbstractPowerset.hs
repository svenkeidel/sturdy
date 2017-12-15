{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
module Data.AbstractPowerset where

import qualified Data.Powerset as P
import           Data.Foldable (toList)
import           Data.List (intercalate)
import           Data.Order
import           Data.Sequence (Seq)

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Deduplicate

newtype Pow a = Pow (Seq a) deriving (Eq, Functor, Applicative, Monad, Alternative, MonadPlus, Monoid, Foldable, Traversable)

instance Show a => Show (Pow a) where
  show (Pow a) = "{" ++ intercalate ", " (show <$> toList a) ++ "}"

instance PreOrd a => PreOrd (Pow a) where
  as ⊑ bs = all (\x -> any (x ⊑) bs) as

instance PreOrd a => Complete (Pow a) where
  as ⊔ bs = as `P.union` bs

instance MonadDeduplicate Pow where
  dedup = P.fromFoldable . P.toHashSet
