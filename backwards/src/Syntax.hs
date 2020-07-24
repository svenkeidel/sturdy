{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Syntax where

import Data.Text(Text,pack,unpack)
import Data.Text.Prettyprint.Doc
import Data.String(IsString(..))

data Expr
  = Var Text
  | NumLit Int
  | Add Expr Expr
  deriving (Eq)

-- These instances allow to use prettier syntax to construct expressions.
instance Num Expr where
  fromInteger n = NumLit (fromInteger n)
  e1 + e2 = Add e1 e2
  (-)    = error "unsupported expression"
  (*)    = error "unsupported expression"
  negate = error "unsupported expression"
  abs    = error "unsupported expression"
  signum = error "unsupported expression"

instance IsString Expr where
  fromString x = Var (pack x)

instance Show Expr where
  showsPrec d e0 = case e0 of
    Var x -> showString (unpack x)
    NumLit n -> shows n
    Add e1 e2 -> showParen (d > addPrec)
      $ showsPrec (addPrec + 1) e1
      . showString " + "
      . showsPrec (addPrec + 1) e2
    where
      addPrec = 10

instance Pretty Expr where
  pretty = viaShow
