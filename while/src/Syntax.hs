{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Syntax where

import Control.Monad.State

import Data.Label

import Data.Text (Text)
import Data.Hashable
import Data.Order
import Data.String

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
  | Lt Expr Expr Label
  deriving (Show,Ord,Eq,Generic)

instance IsString (State Label Expr) where
  fromString x = Var (fromString x) <$> fresh

true :: State Label Expr
true = BoolLit True <$> fresh

false :: State Label Expr
false = BoolLit False <$> fresh

-- boolLit :: Bool -> State Label Expr
-- boolLit b = BoolLit b <$> fresh

-- (&&) :: State Label Expr -> State Label Expr -> State Label Expr
-- (&&) e1 e2 = And <$> e1 <*> e2 <*> fresh

-- (||) :: State Label Expr -> State Label Expr -> State Label Expr
-- (||) e1 e2 = Or <$> e1 <*> e2 <*> fresh

-- not :: State Label Expr -> State Label Expr
-- not e = Not <$> e <*> fresh

-- randomNum :: State Label Expr
-- randomNum = RandomNum <$> fresh

(~=) :: State Label Expr -> State Label Expr -> State Label Expr
e1 ~= e2 = Eq <$> e1 <*> e2 <*> fresh

instance Num (State Label Expr) where
  e1 + e2 = Add <$> e1 <*> e2 <*> fresh
  e1 - e2 = Sub <$> e1 <*> e2 <*> fresh
  e1 * e2 = Mul <$> e1 <*> e2 <*> fresh
  fromInteger n = NumLit (fromInteger n) <$> fresh
  abs = undefined
  signum = undefined

-- (/) :: State Label Expr -> State Label Expr -> State Label Expr
-- (/) e1 e2 = Div <$> e1 <*> e2 <*> fresh

-- (==) :: State Label Expr -> State Label Expr -> State Label Expr
-- (==) e1 e2 = Eq <$> e1 <*> e2 <*> fresh

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
    Lt _ _ l -> l
    
instance Hashable Expr where

instance PreOrd Expr where
  (⊑) = (==)
  (≈) = (==)

data Statement
  = While Expr [Statement] Label
  | If Expr [Statement] [Statement] Label
  | Assign Text Expr Label
  deriving (Show,Ord,Eq,Generic)

while :: State Label Expr -> [State Label Statement] -> State Label Statement
while cond body = While <$> cond <*> sequence body <*> fresh

ifExpr :: State Label Expr -> [State Label Statement] -> [State Label Statement] -> State Label Statement
ifExpr cond ifBranch elseBranch = If <$> cond <*> sequence ifBranch <*> sequence elseBranch <*> fresh

(=:) :: Text -> State Label Expr -> State Label Statement
x =: e = Assign x <$> e <*> fresh


instance HasLabel Statement where
  label s = case s of 
    While _ _ l -> l
    If _ _ _ l -> l
    Assign _ _ l -> l

instance Hashable Statement where

type Prog = [Statement]
