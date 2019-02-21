{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Syntax where

import           Control.Monad.State

import           Data.Label

import           Data.Text (Text,unpack)
import           Data.Hashable
import           Data.Order
import           Data.String
import           Data.Lens (Prism')
import qualified Data.Lens as L

import           GHC.Generics

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
  deriving (Ord,Eq,Generic)

instance Show Expr where
  showsPrec d e0 = case e0 of
    Var x _ -> showString (unpack x)
    BoolLit x _ -> literal x
    And e1 e2 _ -> binOp " && " e1 e2
    Or e1 e2 _ -> binOp " || " e1 e2
    Not e _ -> unOp "!" e
    NumLit x _ -> literal x
    RandomNum _ -> showString "rand"
    Add e1 e2 _ -> binOp " + " e1 e2
    Sub e1 e2 _ -> binOp " - " e1 e2
    Mul e1 e2 _ -> binOp " * " e1 e2
    Div e1 e2 _ -> binOp " / " e1 e2
    Eq e1 e2 _ -> binOp " == " e1 e2
    Lt e1 e2 _ -> binOp " < " e1 e2
    where
      literal :: Show x => x -> ShowS
      literal = shows
      unOp x e = showParen (d > app_prec) $ showString x . showsPrec (app_prec + 1) e
      binOp x e1 e2 = showParen (d > app_prec)
        $ showsPrec (app_prec + 1) e1
        . showString x
        . showsPrec (app_prec + 1) e2
      app_prec = 10

type LExpr = State Label Expr

instance IsString LExpr where
  fromString x = Var (fromString x) <$> fresh

true :: LExpr
true = BoolLit True <$> fresh

false :: LExpr
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

(<) :: LExpr -> LExpr -> LExpr
e1 < e2 = Lt <$> e1 <*> e2 <*> fresh

(~=) :: LExpr -> LExpr -> LExpr
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

instance HasLabel Expr Label where
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
  = While Expr Statement Label
  | If Expr Statement Statement Label
  | Assign Text Expr Label
  | Begin [Statement] Label
  deriving (Ord,Eq,Generic)

instance Show Statement where
  showsPrec _ e0 = case e0 of
    Assign x e _ -> showString (unpack x) . showString " := " . shows e
    While e body _ -> showString "while" . showParen True (shows e) . showString " " . shows body
    If e ifB elseB _ -> showString "if" . showParen True (shows e) . showString " " . shows ifB . showString " " . shows elseB
    Begin ss _ -> shows ss

type LStatement = State Label Statement

while :: LExpr -> [LStatement] -> State Label Statement
while cond body = do
  l <- fresh
  While <$> cond <*> begin body <*> pure l

whileLoops :: Prism' [Statement] ((Expr,Statement,Label),[Statement])
whileLoops = L.prism' (\((c,b,l),ss) -> While c b l:ss)
                (\s -> case s of
                   While c b l:ss -> Just ((c,b,l),ss)
                   _ -> Nothing)

ifExpr :: State Label Expr -> [State Label Statement] -> [State Label Statement] -> State Label Statement
ifExpr cond ifBranch elseBranch = If <$> cond <*> begin ifBranch <*> begin elseBranch <*> fresh

begin :: [LStatement] -> LStatement
begin ss = Begin <$> sequence ss <*> fresh

(=:) :: Text -> State Label Expr -> State Label Statement
x =: e = Assign x <$> e <*> fresh
infix 0 =:

instance HasLabel Statement Label where
  label s = case s of 
    While _ _ l -> l
    If _ _ _ l -> l
    Assign _ _ l -> l
    Begin _ l -> l

instance Hashable Statement where
  hashWithSalt s e = s `hashWithSalt` (label e :: Label)

blocks :: [Statement] -> [Statement]
blocks ss = flip concatMap ss $ \s -> case s of
  Assign {} -> [s]
  If _ s1 s2 _ -> s : blocks [s1, s2]
  While _ body _ -> s : blocks [body]
  Begin ss' _ -> blocks ss'

