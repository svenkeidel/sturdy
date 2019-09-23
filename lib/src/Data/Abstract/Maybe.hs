{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Abstract.Maybe where

import Prelude hiding (Maybe(..))
import qualified Prelude as Con

import Control.DeepSeq
import Control.Monad(ap)
import Control.Arrow(second)

import Data.Order
import Data.Hashable
import Data.Traversable
import Data.Abstract.Stable
import Data.Abstract.Widening
import Data.Empty

import GHC.Generics(Generic)

-- | Abstract 'Maybe' type with an upper bound for 'Just' and 'Nothing'
data Maybe a = Just a | Nothing | JustNothing a
  deriving (Eq,Ord,Show,Generic)

instance NFData a => NFData (Maybe a)

instance (Hashable a) => Hashable (Maybe a) where
  hashWithSalt s (Just a) = s `hashWithSalt` (1::Int) `hashWithSalt` a
  hashWithSalt s Nothing = s `hashWithSalt` (2::Int)
  hashWithSalt s (JustNothing a) = s `hashWithSalt` (3 ::Int) `hashWithSalt` a

instance PreOrd a => PreOrd (Maybe a) where
  Just a ⊑ Just b = a ⊑ b
  Nothing ⊑ Nothing = True
  JustNothing a ⊑ JustNothing b = a ⊑ b
  Just a ⊑ JustNothing b = a ⊑ b
  Nothing ⊑ JustNothing _ = True
  _ ⊑ _ = False

instance Complete a => Complete (Maybe a) where
  Just x ⊔ Just y = Just (x ⊔ y)
  Nothing ⊔ Nothing = Nothing
  Just x ⊔ Nothing = JustNothing x
  Nothing ⊔ Just y = JustNothing y
  Just x ⊔ JustNothing y = JustNothing (x ⊔ y)
  Nothing ⊔ JustNothing y = JustNothing y
  JustNothing x ⊔ Just y = JustNothing (x ⊔ y)
  JustNothing x ⊔ Nothing = JustNothing x
  JustNothing x ⊔ JustNothing y = JustNothing (x ⊔ y)

widening :: Widening a -> Widening (Maybe a)
widening w (Just x) (Just y) = second Just (w x y)
widening _ Nothing Nothing = (Stable,Nothing)
widening _ (Just x) Nothing = (Unstable,JustNothing x)
widening _ Nothing (Just y) = (Unstable,JustNothing y)
widening w (Just x) (JustNothing y) = let (_,z) = w x y in (Unstable,JustNothing z)
widening _ Nothing (JustNothing y) = (Unstable,JustNothing y)
widening w (JustNothing x) (Just y) = let (_,z) = w x y in (Unstable,JustNothing z)
widening _ (JustNothing y) Nothing = (Unstable,JustNothing y)
widening w (JustNothing x) (JustNothing y) = second JustNothing (w x y)

instance UpperBounded a => UpperBounded (Maybe a) where
  top = JustNothing top

instance Functor Maybe where
  fmap f m = case m of
    Just a -> Just (f a)
    Nothing -> Nothing
    JustNothing a -> JustNothing (f a)

instance Applicative Maybe where
  pure = return
  (<*>) = ap

instance IsEmpty (Maybe a) where
  empty = Nothing

instance Monad Maybe where
  return = Just
  x >>= k = case x of
    Nothing -> Nothing
    Just a -> k a
    JustNothing a -> case k a of
      Nothing -> Nothing
      Just a' -> JustNothing a'
      JustNothing a' -> JustNothing a'

instance Foldable Maybe where
  foldMap = foldMapDefault

instance Traversable Maybe where
  traverse _ Nothing = pure Nothing
  traverse f (Just a) = Just <$> f a
  traverse f (JustNothing a) = JustNothing <$> f a

fromConcreteMaybe :: Con.Maybe a -> Maybe a
fromConcreteMaybe m = case m of
  Con.Just a -> Just a
  Con.Nothing -> Nothing

toConcreteMaybe :: Maybe a -> Con.Maybe a
toConcreteMaybe m = case m of
  Just a -> Con.Just a
  JustNothing a -> Con.Just a
  Nothing -> Con.Nothing
