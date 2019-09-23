{-# LANGUAGE FunctionalDependencies #-}
module Data.Term where

import GenericInterpreter (IsTerm(..))

class IsTerm t c => IsAbstractTerm t c where
  wildcard :: c () t

class TermUtils t where
  size :: t -> Int
  height :: t -> Int
  convertToList :: [t] -> t
