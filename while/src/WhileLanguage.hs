{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveGeneric #-}
module WhileLanguage where

import Prelude hiding (lookup, and, or, not, div)

import Data.Label

import Control.Arrow
import Control.Arrow.Fix
import Control.Arrow.Utils
import Control.Arrow.Store
import Control.Arrow.Fail

import Data.Text (Text)
import Data.Hashable
import Data.Order

import GHC.Generics
import System.Random

data Expr
  = Var Text Label
  | BoolLit Bool Label
  | And Expr Expr Label
  | Or Expr Expr Label
  | Not Expr Label
  | NumLit Double Label
  | RandomNum Label
  | Add Expr Expr Label
  | Sub Expr Expr Label
  | Mul Expr Expr Label
  | Div Expr Expr Label
  | Eq Expr Expr Label
  deriving (Show,Ord,Eq,Generic)

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
    
instance Hashable Expr where

instance PreOrd Expr where
  (⊑) = (==)
  (≈) = (==)

data Statement = While Expr [Statement] Label
               | If Expr [Statement] [Statement] Label
               | Assign Text Expr Label
  deriving (Show,Ord,Eq)

instance HasLabel Statement where
  label s = case s of 
    While _ _ l -> l
    If _ _ _ l -> l
    Assign _ _ l -> l

type Prog = [Statement]

class HasRandomGen c where
  nextRandom :: Random a => c () a

class Arrow c => Eval v c | c -> v where
  boolLit :: c (Bool,Label) v
  and :: c (v,v,Label) v
  or :: c (v,v,Label) v
  not :: c (v,Label) v
  numLit :: c (Double,Label) v
  randomNum :: c Label v
  add :: c (v,v,Label) v
  sub :: c (v,v,Label) v
  mul :: c (v,v,Label) v
  div :: c (v,v,Label) v
  eq :: c (v,v,Label) v
  
class FixEval v c | c -> v where
  fixEval :: (c Expr v -> c Expr v) -> c Expr v

eval :: (ArrowFail String c, ArrowStore (Text,Label) v c, ArrowChoice c, FixEval v c, Eval v c) => c Expr v
eval = fixEval $ \ev -> proc e -> case e of
  Var x l -> lookup' -< (x,l)
  BoolLit b l -> boolLit -< (b,l)
  And e1 e2 l -> do
    v1 <- ev -< e1
    v2 <- ev -< e2
    and -< (v1,v2,l)
  Or e1 e2 l -> do
    v1 <- ev -< e1
    v2 <- ev -< e2
    or -< (v1,v2,l)
  Not e1 l -> do
    v1 <- ev -< e1
    not -< (v1,l)
  NumLit n l -> numLit -< (n,l)
  RandomNum l -> randomNum -< l
  Add e1 e2 l -> do
    v1 <- ev -< e1
    v2 <- ev -< e2
    add -< (v1,v2,l)
  Sub e1 e2 l -> do
    v1 <- ev -< e1
    v2 <- ev -< e2
    sub -< (v1,v2,l)
  Mul e1 e2 l -> do
    v1 <- ev -< e1
    v2 <- ev -< e2
    mul -< (v1,v2,l)
  Div e1 e2 l -> do
    v1 <- ev -< e1
    v2 <- ev -< e2
    div -< (v1,v2,l)
  Eq e1 e2 l -> do
    v1 <- ev -< e1
    v2 <- ev -< e2
    eq -< (v1,v2,l)

class Run c v | c -> v where
  if_ :: c [Statement] () -> c [Statement] () -> c (v,([Statement],[Statement]),Label) ()

run :: (ArrowFix [Statement] () c, ArrowFail String c, ArrowChoice c, ArrowStore (Text,Label) v c, Run c v, FixEval v c, Eval v c) => c [Statement] ()
run = fixA $ \run' -> voidA $ mapA $ step run'

step :: (ArrowFail String c, ArrowChoice c, ArrowStore (Text,Label) v c, Run c v, FixEval v c, Eval v c) => c [Statement] () -> c Statement ()
step steps = proc stmt -> case stmt of
  Assign x e l -> do
    v <- eval -< e
    store -< ((x,l),v)
  If cond b1 b2 l -> do
    b <- eval -< cond
    if_ steps steps -< (b,(b1,b2),l)
  While cond body l ->
    steps -< [If cond (body ++ [While cond body l]) [] l]
