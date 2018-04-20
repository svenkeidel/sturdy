module Syntax where

data Expr
  = Var String
  | BoolLit Bool
  | And Expr Expr
  | NumLit Int
  | Add Expr Expr
  | Lt Expr Expr

data Statement
  = Assign String Expr               -- x := y + z
  | If Expr [Statement] [Statement]  -- if(x < y) {x:=1} else {y:=2}
  | While Expr [Statement]
