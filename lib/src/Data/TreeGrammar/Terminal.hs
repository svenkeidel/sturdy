{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Data.TreeGrammar.Terminal(Terminal(..)) where

import           Prelude hiding (pred,traverse,map,Either(..))
import           Control.Monad

import           Data.HashSet (HashSet)
import           Data.Functor.Identity
import           Data.Identifiable

class Terminal t where
  nonTerminals :: Identifiable n => t n -> HashSet n
  productive :: Identifiable n => HashSet n -> t n -> Bool
  determinize :: (Identifiable n, Identifiable n', Applicative f) => (HashSet n -> f n') -> t n -> f (t n')
  subsetOf :: (Identifiable n, Identifiable n', MonadPlus f) => ((HashSet [n],HashSet [n']) -> f ()) -> t n -> t n' -> f ()

  map :: Identifiable n' => (n -> n') -> t n -> t n'
  map f t = runIdentity (traverse (Identity . f) t)
  traverse :: (Identifiable n', Applicative f) => (n -> f n') -> t n -> f (t n')
  filter :: (a -> Bool) -> t a -> t a

  intersection :: (Identifiable n1, Identifiable n2, Identifiable n', Applicative f) => ((n1,n2) -> f n') -> t n1 -> t n2 -> f (t n')
  hashWithSalt :: Monad f => (Int -> n -> f Int) -> Int -> t n -> f Int
