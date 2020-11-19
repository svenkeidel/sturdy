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



-- Helper functions to generate labled expressions and statements -------
var :: String -> LExpr
var x = Var x <$> fresh

boolLit :: Bool -> LExpr
boolLit b = BoolLit b <$> fresh

and :: LExpr -> LExpr -> LExpr
and e1 e2 = And <$> e1 <*> e2 <*> fresh

numLit :: Int -> LExpr
numLit n = NumLit n <$> fresh

add :: LExpr -> LExpr -> LExpr
add e1 e2 = Add <$> e1 <*> e2 <*> fresh

lt :: LExpr -> LExpr -> LExpr
lt e1 e2 = Lt <$> e1 <*> e2 <*> fresh

assign :: String -> LExpr -> LStatement
assign x e = Assign x <$> e <*> fresh

if' :: LExpr -> [LStatement] -> [LStatement] -> LStatement
if' e s1 s2 = If <$> e <*> sequence s1 <*> sequence s2 <*> fresh

--lst s = sequence s <*> fresh

while :: LExpr -> [LStatement] -> LStatement
while e body = While <$> e <*> sequence body <*> fresh


isWhileLoop :: ((env,[Statement]),store) -> Bool
isWhileLoop ((_, s),_) = case s of
  While {} : _ -> True
  _            -> False


-- Instances ------------------------------------------------------------

type LExpr = State Label Expr
instance HasLabel Expr where
  label e = case e of 
    Var _ l -> l
    BoolLit _ l -> l
    And _ _ l -> l
    NumLit _ l -> l
    Add _ _ l -> l
    Lt _ _ l -> l
instance Hashable Expr

type LStatement = State Label Statement
instance HasLabel Statement where
  label s = case s of 
    While _ _ l -> l
    If _ _ _ l -> l
    Assign _ _ l -> l
instance Hashable Statement
