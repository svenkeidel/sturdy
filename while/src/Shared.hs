{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
module Shared where

import Prelude hiding (lookup, and, or, not, div, read)

import Data.Label

import Control.Arrow
import Control.Arrow.Fix
import Control.Arrow.Utils
import Control.Arrow.Store

import Data.Text (Text)

import Expressions

type Prog = [Statement]
  
eval :: (ArrowChoice c, HasStore v c, IsVal v c) => c Expr v
eval = proc e -> case e of
  Var x l -> lookup -< (x,l)
  BoolLit b l -> boolLit -< (b,l)
  And e1 e2 l -> do
    v1 <- eval -< e1
    v2 <- eval -< e2
    and -< (v1,v2,l)
  Or e1 e2 l -> do
    v1 <- eval -< e1
    v2 <- eval -< e2
    or -< (v1,v2,l)
  Not e1 l -> do
    v1 <- eval -< e1
    not -< (v1,l)
  NumLit n l -> numLit -< (n,l)
  RandomNum l -> randomNum -< l
  Add e1 e2 l -> do
    v1 <- eval -< e1
    v2 <- eval -< e2
    add -< (v1,v2,l)
  Sub e1 e2 l -> do
    v1 <- eval -< e1
    v2 <- eval -< e2
    sub -< (v1,v2,l)
  Mul e1 e2 l -> do
    v1 <- eval -< e1
    v2 <- eval -< e2
    mul -< (v1,v2,l)
  Div e1 e2 l -> do
    v1 <- eval -< e1
    v2 <- eval -< e2
    div -< (v1,v2,l)
  Eq e1 e2 l -> do
    v1 <- eval -< e1
    v2 <- eval -< e2
    eq -< (v1,v2,l)

run :: (ArrowFix [Statement] () c, ArrowChoice c, Run v c, HasStore v c, IsVal v c) => c [Statement] ()
run = fixA $ \run' -> voidA $ mapA $ proc stmt -> case stmt of
  Assign x e l -> do
    v <- eval -< e
    store -< (x,v,l)
  If cond b1 b2 l -> do
    b <- eval -< cond
    if_ run' run' -< (b,(b1,b2),l)
  While cond body l ->
    run' -< [If cond (body ++ [While cond body l]) [] l]

class ArrowStore Text v c => HasStore v c | c -> v where
  lookup :: c (Text,Label) v
  lookup = proc (x,_) -> read -< x
  store :: c (Text,v,Label) ()
  store = proc (x,v,_) -> write -< (x,v)

class Arrow c => IsVal v c | c -> v where
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

class ArrowStore Text v c => Run v c | c -> v where
  if_ :: c [Statement] () -> c [Statement] () -> c (v,([Statement],[Statement]),Label) ()
