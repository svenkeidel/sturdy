module Syntax where

data Expr
  = Var String                       -- x
  | BoolLit Bool                     -- true, false
  | And Expr Expr                    -- p && q
  | NumLit Int                       -- 5
  | Add Expr Expr                    -- n + m
  | Lt Expr Expr                     -- n < m

data Statement
  = Assign String Expr               -- x := y + z
  | If Expr [Statement] [Statement]  -- if(x < y) {x:=1} else {y:=2}
  | While Expr [Statement]           -- while(x < 10) {x:=1}

