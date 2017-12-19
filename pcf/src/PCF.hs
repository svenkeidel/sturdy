module PCF where

import Data.Text(Text)

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
