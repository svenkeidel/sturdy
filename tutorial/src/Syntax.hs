{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
module Syntax where

import           Control.Monad.State
import           Data.Label
import           Data.Hashable
import           GHC.Generics

data Expr
  = Var String Label                      -- x
  | BoolLit Bool Label                    -- true, false
  | And Expr Expr Label                   -- p && q
  | NumLit Int Label                      -- 5
  | Add Expr Expr Label                   -- n + m
  | Lt Expr Expr Label                    -- n < m
  deriving (Show,Eq,Generic)

data Statement
  = Assign String Expr Label              -- x := y + z
  | If Expr [Statement] [Statement] Label -- if(x < y) {x:=1} else {y:=2}
  | While Expr [Statement] Label          -- while(x < 10) {x:=1}
  deriving (Show,Eq,Generic)

------------ Instances --------------

type LExpr = State Label Expr
instance HasLabel Expr Label where
  label e = case e of 
    Var _ l -> l
    BoolLit _ l -> l
    And _ _ l -> l
    NumLit _ l -> l
    Add _ _ l -> l
    Lt _ _ l -> l
instance Hashable Expr

type LStatement = State Label Statement
instance HasLabel Statement Label where
  label s = case s of 
    While _ _ l -> l
    If _ _ _ l -> l
    Assign _ _ l -> l
instance Hashable Statement
