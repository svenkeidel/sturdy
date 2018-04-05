{-# LANGUAGE DeriveGeneric #-}
module Expressions where

import Data.Label

import Data.Text (Text)
import Data.Hashable
import Data.Order

import GHC.Generics

data Expr
  = Var Text Label
  | BoolLit Bool Label
  | And Expr Expr Label
  | Or Expr Expr Label
  | Not Expr Label
  | NumLit Int Label
  | RandomNum Label
  | Add Expr Expr Label
  | Sub Expr Expr Label
  | Mul Expr Expr Label
  | Div Expr Expr Label
  | Eq Expr Expr Label
  deriving (Show,Ord,Eq,Generic)

instance HasLabel Expr where
  label e = case e of 
    Var _ l -> l
    BoolLit _ l -> l
    And _ _ l -> l
    Or _ _ l -> l
    Not _ l -> l
    NumLit _ l -> l
    RandomNum l -> l
    Add _ _ l -> l
    Sub _ _ l -> l
    Mul _ _ l -> l
    Div _ _ l -> l
    Eq _ _ l -> l
    
instance Hashable Expr where

instance PreOrd Expr where
  (⊑) = (==)
  (≈) = (==)

data Statement
  = While Expr [Statement] Label
  | If Expr [Statement] [Statement] Label
  | Assign Text Expr Label
  deriving (Show,Ord,Eq,Generic)

instance HasLabel Statement where
  label s = case s of 
    While _ _ l -> l
    If _ _ _ l -> l
    Assign _ _ l -> l

instance Hashable Statement where

type Prog = [Statement]
