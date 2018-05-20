{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module SharedSemantics where

import Prelude hiding (id,lookup, and, or, not, div, read)

import Data.Label

import Control.Arrow
import Control.Category
import Control.Arrow.Environment
import Control.Arrow.Fix
import Control.Arrow.Store
import Control.Arrow.Try

import Data.Text (Text)

import Syntax

type Prog = [Statement]

eval :: (ArrowChoice c, ArrowEnv Text a env c, ArrowStore a v Label c, IsVal v a c) => c Expr v
eval = proc e -> case e of
  Var x l -> do
    a <- lookup -< x
    read -< (a,l)
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
  Newref e1 l -> do
    v <- eval -< e1
    a <- freshAddr -< l
    write -< (a,v,l)
    ref -< a
  Deref e1 l -> do
    v <- eval -< e1
    a <- getAddr -< (v,l)
    read -< (a,l)


run :: (
  ArrowFix [Statement] () c,
  ArrowChoice c,
  ArrowEnv Text a env c,
  ArrowStore a v Label c,
  ArrowTry (Text,Label) a a c,
  Conditional v [Statement] [Statement] () c,
  IsVal v a c
  ) => c [Statement] ()
run = fixA $ \run' -> proc stmts -> case stmts of
  (Assign x e l:ss) -> do
    v <- eval -< e
    a <- tryA (proc (x,_) -> lookup -< x)    -- lookup up address of x
              id                             -- if successful, return that address
              (proc (_,l) -> freshAddr -< l) -- otherwise create fresh address from l
           -< (x,l)
    write -< (a,v,l)
    extendEnv' run' -< (x, a, ss) -- extend environment in case `a` is fresh
  (Set x e l:ss) -> do
    v <- eval -< e
    xa <- lookup -< x
    r <- read -< (xa,l)
    a <- getAddr -< (r,l)
    write -< (a,v,l)
    run' -< ss
  (If cond b1 b2 _:ss) -> do
    b <- eval -< cond
    if_ run' run' -< (b,(b1,b2))
    run' -< ss
  (While cond body l:ss) ->
    run' -< If cond (body ++ [While cond body l]) [] l : ss
  [] ->
    returnA -< ()

class Arrow c => IsVal v a c | c -> v, c -> a where
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
  freshAddr :: c Label a
  ref :: c a v
  getAddr :: c (v,Label) a

class Arrow c => Conditional v x y z c | c -> v where
  if_ :: c x z -> c y z -> c (v,(x,y)) z
