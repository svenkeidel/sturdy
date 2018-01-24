{-# LANGUAGE FunctionalDependencies #-}
module Data.Term where

import Data.Constructor
import Data.Text(Text)

class IsTerm t c | c -> t where
  matchTermAgainstConstructor :: c ([t'],[t]) [t] -> c (Constructor, [t'], t) t 
  matchTermAgainstString :: c (Text,t) t
  matchTermAgainstNumber :: c (Int,t) t
  matchTermAgainstExplode :: c t t -> c t t -> c t t
  equal :: c (t,t) t
  convertFromList :: c (t,t) t
  mapSubterms :: c [t] [t] -> c t t

  cons :: c (Constructor,[t]) t
  numberLiteral :: c Int t
  stringLiteral :: c Text t

class IsTerm t c => IsAbstractTerm t c where
  wildcard :: c () t

class TermUtils t where
  size :: t -> Int
  height :: t -> Int
  convertToList :: [t] -> t
