{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module SharedSemantics where

import Prelude hiding (lookup, and, or, not, div, read)

import Data.Label

import Control.Arrow
import Control.Arrow.Fix
import Control.Arrow.Store
import Control.Arrow.Utils

import Data.Text (Text)

import Syntax

type Prog = [Statement]
  
eval :: (ArrowChoice c, ArrowStore Text v Label c, IsVal v c) => c Expr v
eval = proc e -> case e of
  Var x l -> read -< (x,l)
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
  Lt e1 e2 l -> do
    v1 <- eval -< e1
    v2 <- eval -< e2
    lt -< (v1,v2,l)

run :: (ArrowFix Statement () c, ArrowChoice c, ArrowStore Text v Label c,
        Conditional v Statement Statement () c, IsVal v c)
    => c Statement ()
run = fixA $ \run' -> proc stmts -> case stmts of
  Assign x e l -> do
    v <- eval -< e
    write -< (x,v,l)
  If cond b1 b2 _ -> do
    b <- eval -< cond
    if_ run' run' -< (b,(b1,b2))
  While cond body l ->
    run' -< If cond (Begin [body,While cond body l] l) (Begin [] l) l
  Begin ss _ ->
    voidA $ mapA run' -< ss

class Arrow c => IsVal v c | c -> v where
  boolLit :: c (Bool,Label) v
  and :: c (v,v,Label) v
  or :: c (v,v,Label) v
  not :: c (v,Label) v
  numLit :: c (Int,Label) v
  randomNum :: c Label v
  add :: c (v,v,Label) v
  sub :: c (v,v,Label) v
  mul :: c (v,v,Label) v
  div :: c (v,v,Label) v
  eq :: c (v,v,Label) v
  lt :: c (v,v,Label) v

class Arrow c => Conditional v x y z c | c -> v where
  if_ :: c x z -> c y z -> c (v,(x,y)) z
