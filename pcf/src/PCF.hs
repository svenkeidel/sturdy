module PCF where

import Data.Text(Text,unpack)
import Data.Hashable
import Data.String

data Expr
  = Var Text
  | Lam Text Expr
  | App Expr Expr
  | Zero
  | Succ Expr
  | Pred Expr
  | IfZero Expr Expr Expr
  | Y Expr
  deriving (Eq)

instance Show Expr where
  showsPrec d e0 = case e0 of
    Var x -> showString (unpack x)
    Zero -> showString "zero"
    Succ e -> showParen (d > app_prec) $ showString "succ " . showsPrec (app_prec + 1) e
    Pred e -> showParen (d > app_prec) $ showString "pred " . showsPrec (app_prec + 1) e
    Y e -> showParen (d > app_prec) $ showString "Y " . showsPrec (app_prec + 1) e
    IfZero e1 e2 e3 -> showParen (d > app_prec)
      $ showString "ifZero "
      . showsPrec (app_prec + 1) e1
      . showString " "
      . showsPrec (app_prec + 1) e2
      . showString " "
      . showsPrec (app_prec + 1) e3
    App e1 e2 -> showParen (d > app_prec)
      $ showsPrec (app_prec + 1) e1
      . showString " "
      . showsPrec (app_prec + 1) e2
    Lam x e2 -> showParen (d > lam_prec)
      $ showString "λ"
      . showString (unpack x)
      . showString ". "
      . shows e2
    where
      app_prec = 10
      lam_prec = 9


instance IsString Expr where
  fromString = Var . fromString

instance Hashable Expr where
  hashWithSalt s (Var x) = s `hashWithSalt` (0::Int) `hashWithSalt` x
  hashWithSalt s (Lam x e) = s `hashWithSalt` (1::Int) `hashWithSalt` x `hashWithSalt` e
  hashWithSalt s (App e1 e2) = s `hashWithSalt` (2::Int) `hashWithSalt` e1 `hashWithSalt` e2
  hashWithSalt s Zero = s `hashWithSalt` (3::Int)
  hashWithSalt s (Succ e) = s `hashWithSalt` (4::Int) `hashWithSalt` e
  hashWithSalt s (Pred e) = s `hashWithSalt` (5::Int) `hashWithSalt` e
  hashWithSalt s (IfZero e1 e2 e3) = s `hashWithSalt` (6::Int) `hashWithSalt` e1 `hashWithSalt` e2 `hashWithSalt` e3
  hashWithSalt s (Y e) = s `hashWithSalt` (7::Int) `hashWithSalt` e
