{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveGeneric #-}
module WhileLanguage(module Label, module WhileLanguage) where

import Prelude hiding (lookup,fail,and,or,not,div)

import Label

import Control.Arrow
import Data.Text (Text)
import Data.Hashable
import Data.Order

import GHC.Generics

data Expr = Var Text
          | BoolLit Bool
          | And Expr Expr
          | Or Expr Expr
          | Not Expr
          | NumLit Double
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Eq Expr Expr
  deriving (Show,Ord,Eq,Generic)

instance Hashable Expr where

instance PreOrd Expr where
  (⊑) = (==)
  (≈) = (==)

data Statement = While Expr [Statement] Label
               | If Expr [Statement] [Statement] Label
               | Assign Text Expr Label
  deriving (Show,Ord,Eq)

label :: Statement -> Label
label (While _ _ l) = l
label (If _ _ _ l) = l
label (Assign _ _ l) = l

type Prog = [Statement]

class ArrowChoice c => HasStore c st where
  getStore :: c a st
  putStore :: c st ()
  modifyStore :: c (st -> st) ()
  modifyStore = proc f -> do
    store <- getStore -< ()
    putStore -< f store

class ArrowChoice c => HasProp c pr where
  getProp :: c () pr
  putProp :: c pr ()
  modifyProp :: c (pr -> pr) ()

class ArrowChoice c => Eval c v | c -> v where
  lookup :: c Text v
  boolLit :: c Bool v
  and :: c (v,v) v
  or :: c (v,v) v
  not :: c v v
  numLit :: c Double v
  add :: c (v,v) v
  sub :: c (v,v) v
  mul :: c (v,v) v
  div :: c (v,v) v
  eq :: c (v,v) v
  fixEval :: (c Expr v -> c Expr v) -> c Expr v

eval :: Eval c v => c Expr v
eval = fixEval $ \ev -> proc e -> case e of
  Var x -> lookup -< x
  BoolLit b -> boolLit -< b
  And e1 e2 -> do
    v1 <- ev -< e1
    v2 <- ev -< e2
    and -< (v1,v2)
  Or e1 e2 -> do
    v1 <- ev -< e1
    v2 <- ev -< e2
    or -< (v1,v2)
  Not e1 -> do
    v1 <- ev -< e1
    not -< v1
  NumLit n -> numLit -< n
  Add e1 e2 -> do
    v1 <- ev -< e1
    v2 <- ev -< e2
    add -< (v1,v2)
  Sub e1 e2 -> do
    v1 <- ev -< e1
    v2 <- ev -< e2
    sub -< (v1,v2)
  Mul e1 e2 -> do
    v1 <- ev -< e1
    v2 <- ev -< e2
    mul -< (v1,v2)
  Div e1 e2 -> do
    v1 <- ev -< e1
    v2 <- ev -< e2
    div -< (v1,v2)
  Eq e1 e2 -> do
    v1 <- ev -< e1
    v2 <- ev -< e2
    eq -< (v1,v2)

class Run c v | c -> v where
  store :: c (Text,v,Label) ()
  if_ :: c [Statement] () -> c [Statement] () -> c (v,([Statement],[Statement]),Label) ()
  fixRun :: (c [Statement] () -> c Statement ()) -> c [Statement] ()

run :: (Run c v, Eval c v) => c [Statement] ()
run = fixRun $ \run' -> proc stmt -> case stmt of
  Assign x e l -> do
    v <- eval -< e
    store -< (x,v,l)
  If cond b1 b2 l -> do
    b <- eval -< cond
    if_ run' run' -< (b,(b1,b2),l)
  While cond body l ->
    run' -< [If cond (body ++ [While cond body l]) [] l]
