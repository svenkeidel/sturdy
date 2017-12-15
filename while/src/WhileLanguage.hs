{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Arrows #-}
module WhileLanguage where

import Prelude hiding (lookup,fail,and,or,not,div)

import Control.Arrow
import Data.Text (Text)

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
  deriving (Show,Ord,Eq)

data Statement = While Expr [Statement]
               | If Expr [Statement] [Statement]
               | Assign Text Expr
  deriving (Show,Ord,Eq)

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
  store :: c (Text,v) ()
  if_ :: c [Statement] () -> c [Statement] () -> c (v,([Statement],[Statement])) ()
  fixRun :: (c [Statement] () -> c Statement ()) -> c [Statement] ()

run :: (Run c v, Eval c v) => c [Statement] ()
run = fixRun $ \run' -> proc stmt -> case stmt of
  Assign x e -> do
    v <- eval -< e
    store -< (x,v) 
  If cond b1 b2 -> do
    b <- eval -< cond
    if_ run' run' -< (b,(b1,b2))
  While cond body ->
    run' -< [If cond (body ++ [While cond body]) []]

mapA :: ArrowChoice c => c x y -> c [x] [y]
mapA f = proc l -> case l of
  (x:xs) -> do
      y <- f -< x
      ys <- mapA f -< xs
      returnA -< (y:ys)
  [] -> returnA -< []

voidA :: ArrowChoice c => c x y -> c x ()
voidA f = proc x -> do
  _ <- f -< x
  returnA -< ()
