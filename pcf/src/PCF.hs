module PCF where

import Data.Text(Text)
import Data.Hashable



data Expr = Var Text
          | Lam Text Type Expr
          | App Expr Expr
          | Zero
          | Succ Expr
          | Pred Expr
          | IfZero Expr Expr Expr
          | Y Expr deriving (Eq, Show)

data Type = NumT
          | FunT Type Type deriving (Eq, Show)

instance Hashable Expr where
  hashWithSalt s (Var x) = s `hashWithSalt` (0::Int) `hashWithSalt` x
  hashWithSalt s (Lam x t e) = s `hashWithSalt` (1::Int) `hashWithSalt` x `hashWithSalt` t `hashWithSalt` e
  hashWithSalt s (App e1 e2) = s `hashWithSalt` (2::Int) `hashWithSalt` e1 `hashWithSalt` e2
  hashWithSalt s Zero = s `hashWithSalt` (3::Int)
  hashWithSalt s (Succ e) = s `hashWithSalt` (4::Int) `hashWithSalt` e
  hashWithSalt s (Pred e) = s `hashWithSalt` (5::Int) `hashWithSalt` e
  hashWithSalt s (IfZero e1 e2 e3) = s `hashWithSalt` (6::Int) `hashWithSalt` e1 `hashWithSalt` e2 `hashWithSalt` e3
  hashWithSalt s (Y e) = s `hashWithSalt` (7::Int) `hashWithSalt` e

instance Hashable Type where
  hashWithSalt s NumT = s `hashWithSalt` (0::Int)
  hashWithSalt s (FunT t1 t2) = s `hashWithSalt` (1::Int) `hashWithSalt` t1 `hashWithSalt` t2
