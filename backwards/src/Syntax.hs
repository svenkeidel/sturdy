{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Syntax where

import Control.DeepSeq

import Data.Text(Text,pack,unpack)
import Data.Text.Prettyprint.Doc
import Data.String(IsString(..))

import GHC.Generics

data Expr
  = Var Text
  | NumLit Int
  | BoolLit Bool
  | TypeLit Type
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | And Expr Expr
  | Or Expr Expr
  | Not Expr
  | Equals Expr Expr 
  | If Expr Expr Expr
  | Leq Expr Expr
  | TypeOf Expr Expr
  deriving (Eq, Generic, NFData)

data Type = Boolean | Number
  deriving (Eq, Generic, NFData)



-- These instances allow to use prettier syntax to construct expressions.
instance Num Expr where
  fromInteger n = NumLit (fromInteger n)
  e1 + e2 = Add e1 e2
  e1 - e2 = Sub e1 e2
  e1 * e2 = Mul e1 e2
  negate = error "unsupported expression"
  abs    = error "unsupported expression"
  signum = error "unsupported expression"

instance IsString Expr where
  fromString x = Var (pack x)

instance Show Expr where
  showsPrec d e0 = case e0 of
    Var x -> showString (unpack x)
    NumLit n -> shows n
    TypeLit Boolean -> showString "Boolean" 
    TypeLit Number  -> showString "Number"
    Add e1 e2 -> showParen (d > addPrec)
      $ showsPrec (addPrec + 1) e1
      . showString " + "
      . showsPrec (addPrec + 1) e2
    Sub e1 e2 -> showParen (d > addPrec)
      $ showsPrec (addPrec + 1) e1
      . showString " - "
      . showsPrec (addPrec + 1) e2
    Mul e1 e2 -> showParen (d > addPrec)
      $ showsPrec (addPrec + 1) e1
      . showString " * "
      . showsPrec (addPrec + 1) e2
    And e1 e2 -> showParen (d > addPrec)
      $ showsPrec (addPrec + 1) e1
      . showString " and "
      . showsPrec (addPrec + 1) e2
    Or e1 e2 -> showParen (d > addPrec)
      $ showsPrec (addPrec + 1) e1
      . showString " or "
      . showsPrec (addPrec + 1) e2
    Equals e1 e2 -> showParen (d > addPrec)
      $ showsPrec (addPrec + 1) e1
      . showString " == "
      . showsPrec (addPrec + 1) e2
    Leq e1 e2 -> showParen (d > addPrec)
      $ showsPrec (addPrec + 1) e1
      . showString " =< "
      . showsPrec (addPrec + 1) e2
    TypeOf e1 e2 -> showParen (d > addPrec)
      $ showsPrec (addPrec + 1) e1
      . showString " typeOf "
      . showsPrec (addPrec + 1) e2
    Not e1 -> showString " not "
      . showsPrec (addPrec + 1) e1
    BoolLit b -> shows b
    If e1 e2 e3 -> showParen (d > ifPrec)
      $ showString "if "
      . showsPrec (ifPrec + 1) e1
      . showString " "
      . showsPrec (ifPrec + 1) e2
      . showString " "
      . showsPrec (ifPrec + 1) e3
    where
      addPrec = 10
      ifPrec = 10

instance Pretty Expr where
  pretty = viaShow

(&&) :: Expr -> Expr -> Expr
e1 && e2 = And e1 e2

(||) :: Expr -> Expr -> Expr
e1 || e2 = Or e1 e2

not :: Expr -> Expr
not e = Not e

(==) :: Expr -> Expr -> Expr
e1 == e2 = Equals e1 e2

(<=) :: Expr -> Expr -> Expr
e1 <= e2 = Leq e1 e2

typeof :: Expr -> Expr -> Expr
typeof e t = TypeOf e t

true :: Expr
true = BoolLit True

false :: Expr
false = BoolLit False

number :: Expr
number = TypeLit Number

boolean :: Expr
boolean = TypeLit Boolean

