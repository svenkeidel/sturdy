{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Abstract.DiscretePowerset where

import           Control.DeepSeq

import           Data.Order
import           Data.HashSet (HashSet)
import qualified Data.HashSet as H
import           Data.Hashable
import           Data.Identifiable
import           Data.List (intercalate)
import           Data.GaloisConnection
import qualified Data.Concrete.Powerset as C
import qualified Data.Empty as E
import qualified Data.Singleton as S

import           Data.Abstract.Widening
import           Data.Abstract.Stable
import           Data.Abstract.FreeCompletion (FreeCompletion)
import qualified Data.Abstract.FreeCompletion as F

import           GHC.Generics
import           GHC.Exts

data Pow x = Pow (HashSet x) | Top deriving (Eq,Generic)
instance NFData x => NFData (Pow x)

empty :: Pow a
empty = Pow H.empty

singleton :: Identifiable x => x -> Pow x
singleton l = Pow (H.singleton l)

union :: Identifiable x => Pow x -> Pow x -> Pow x
union Top _ = Top
union _ Top = Top
union (Pow xs) (Pow ys) = Pow (H.union xs ys)

unions :: Identifiable x => Pow (Pow x) -> Pow x
unions (Pow xs) = foldr union empty xs
unions Top = Top

insert :: Identifiable x => x -> Pow x -> Pow x
insert x (Pow xs) = Pow (H.insert x xs)
insert _ Top = Top

delete :: Identifiable x => x -> Pow x -> Pow x
delete x (Pow xs) = Pow (H.delete x xs)
delete _ Top = Top -- Less precise than it could be.

map :: Identifiable y => (x -> y) -> Pow x -> Pow y
map f (Pow xs) = Pow (H.map f xs)
map _ Top = Top

fromMaybe :: Identifiable x => Maybe x -> Pow x
fromMaybe m = case m of
  Just x  -> singleton x
  Nothing -> empty

instance Show a => Show (Pow a) where
  show (Pow a) = "{" ++ intercalate ", " (show <$> H.toList a) ++ "}"
  show Top = "⊤"

instance Identifiable x => PreOrd (Pow x) where
  Pow xs ⊑ Pow ys = all (\x -> H.member x ys) xs
  _ ⊑ Top = True
  _ ⊑ _ = False

instance Identifiable x => Complete (Pow x) where
  (⊔) = union

instance Identifiable x => Semigroup (Pow x) where
  (<>) = union

instance E.IsEmpty (Pow x) where
  empty = empty

instance Identifiable x => S.IsSingleton (Pow x) where
  type Elem (Pow x) = x
  singleton = singleton

widening :: Identifiable x => Widening (Pow x)
widening (Pow xs) (Pow ys) = let zs = H.union xs ys in (if H.size zs == H.size xs then Stable else Unstable,Pow zs)
widening Top (Pow _) = (Unstable,Top)
widening (Pow _) Top = (Unstable,Top)
widening Top Top = (Stable,Top)
{-# INLINABLE widening #-}

instance Identifiable x => Complete (FreeCompletion (Pow x)) where
  F.Top ⊔ _ = F.Top
  _ ⊔ F.Top = F.Top
  F.Lower xs ⊔ F.Lower ys = F.Lower (xs ⊔ ys)

instance Hashable x => Hashable (Pow x)

instance Identifiable x => UpperBounded (Pow x) where
  top = Top

instance Identifiable x => LowerBounded (Pow x) where
  bottom = Pow H.empty

instance Identifiable x => IsList (Pow x) where
  type Item (Pow x) = x
  fromList ls = Pow (H.fromList ls)
  toList (Pow xs) = H.toList xs
  toList Top = error "toList ⊤"

instance (IsString x, Identifiable x) => IsString (Pow x) where
  fromString s = Pow (H.singleton (fromString s))

instance Identifiable a => Galois (C.Pow a) (Pow a) where
  alpha xs = Pow (C.toHashSet xs)
  gamma = fromList . toList

