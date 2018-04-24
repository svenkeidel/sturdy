{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Arrows #-}
module SharedSemantics where

import Prelude hiding (lookup,and)

import Control.Arrow

import Syntax

class Arrow c => IsValue v c | c -> v where
  lookup :: c String v
  store :: c (String,v) ()
  numLit :: c Int v
  boolLit :: c Bool v
  add :: c (v,v) v
  and :: c (v,v) v
  lt :: c (v,v) v
  if_ :: c [Statement] () -> c [Statement] () -> c (v,[Statement],[Statement]) ()

eval :: (ArrowChoice c, IsValue v c) => c Expr v
eval = proc e -> case e of
  Var x -> lookup -< x
  NumLit n -> numLit -< n
  Add e1 e2 -> do
    v1 <- eval -< e1
    v2 <- eval -< e2
    add -< (v1,v2)
  BoolLit b -> boolLit -< b
  And e1 e2 -> do
    v1 <- eval -< e1
    v2 <- eval -< e2
    and -< (v1,v2)
  Lt e1 e2 -> do
    v1 <- eval -< e1
    v2 <- eval -< e2
    lt -< (v1,v2)

run :: (ArrowChoice c, IsValue v c) => c [Statement] ()
run = proc stmts -> case stmts of
  (Assign x e : rest) -> do
    v <- eval -< e
    store -< (x,v)
    run -< rest
  (If cond ifBranch elseBranch : rest) -> do
    v <- eval -< cond
    if_ run run -< (v,ifBranch,elseBranch)
    run -< rest
  (While cond body : rest) ->
    run -< (If cond (body ++ [While cond body]) [] : rest)
  [] ->
    returnA -< ()

-- instance IsValue Val Interp where
--   ...
--
-- concreteRun :: Interp [Statement] ()
-- concreteRun = run


-- Instance IsValue AbsVal AbsInterp where
--   ...
--
-- absRun :: AbsInterp [Statement] ()
-- absRun = run
