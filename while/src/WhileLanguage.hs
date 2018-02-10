{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveGeneric #-}
module WhileLanguage(module Label, module WhileLanguage) where

import Prelude hiding (lookup, and, or, not, div)

import Label

import Control.Arrow
import Control.Arrow.Fix
import Control.Arrow.Utils

import Data.Text (Text)
import Data.Hashable
import Data.Order

import GHC.Generics
import System.Random

data Expr = Var Text Label
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

class Arrow c => HasStore c st where
  getStore :: c () st
  putStore :: c st ()
  modifyStore :: (c (x,st) st) -> c x ()
  modifyStore f = proc x -> do
    st <- getStore -< ()
    putStore <<< f -< (x,st)

class Arrow c => HasProp c pr where
  getProp :: c () pr
  putProp :: c pr ()
  modifyProp :: (c (x,pr) pr) -> c x ()
  modifyProp f = proc x -> do
    pr <- getProp -< ()
    putProp <<< f -< (x,pr)

class HasRandomGen c where
  nextRandom :: Random a => c () a

class ArrowChoice c => Eval c v | c -> v where
  lookup :: c (Text,Label) v
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
  fixEval :: (c Expr v -> c Expr v) -> c Expr v

eval :: Eval c v => c Expr v
eval = fixEval $ \ev -> proc e -> case e of
  Var x l -> lookup -< (x,l)
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
  store :: c (Text,v,Label) ()
  if_ :: c [Statement] () -> c [Statement] () -> c (v,([Statement],[Statement]),Label) ()

run :: (Run c v, Eval c v, ArrowFix [Statement] () c) => c [Statement] ()
run = fixA $ \run' -> voidA $ mapA $ step run'

step :: (Run c v, Eval c v) => c [Statement] () -> c Statement ()
step steps = proc stmt -> case stmt of
  Assign x e l -> do
    v <- eval -< e
    store -< (x,v,l)
  If cond b1 b2 l -> do
    b <- eval -< cond
    if_ steps steps -< (b,(b1,b2),l)
  While cond body l ->
    steps -< [If cond (body ++ [While cond body l]) [] l]
