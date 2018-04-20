{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module SharedMonadicInterpreter where

import Prelude hiding (lookup,and)

data Expr
  = Var String
  | BoolLit Bool
  | And Expr Expr
  | NumLit Int
  | Add Expr Expr
  | Lt Expr Expr

class Monad m => IsValue v m | m -> v where
  lookup :: String -> m v
  store :: String -> v -> m ()
  numLit :: Int -> m v
  boolLit :: Bool -> m v
  add :: v -> v -> m v
  and :: v -> v -> m v
  lt :: v -> v -> m v
  if_ :: v -> m () -> m () -> m ()

eval :: IsValue v m => Expr -> m v
eval e = case e of
  Var x -> lookup x
  NumLit n -> numLit n
  Add e1 e2 -> do
    v1 <- eval e1
    v2 <- eval e2
    add v1 v2
  BoolLit b -> boolLit b
  And e1 e2 -> do
    v1 <- eval e1
    v2 <- eval e2
    and v1 v2
  Lt e1 e2 -> do
    v1 <- eval e1
    v2 <- eval e2
    lt v1 v2

data Statement
  = Assign String Expr               -- x := y + z
  | If Expr [Statement] [Statement]  -- if(x < y) {x:=1} else {y:=2}
  | While Expr [Statement]

run :: IsValue v m => [Statement] -> m ()
run stmts = case stmts of
  (Assign x e : rest) -> do
    v <- eval e
    store x v
    run rest
  (If cond ifBranch elseBranch : rest) -> do
    v <- eval cond
    if_ v (run ifBranch) (run elseBranch)
    run rest
  (While cond body : rest) ->
    run (If cond (body ++ [While cond body]) [] : rest)
  [] ->
    return ()

-- instance IsValue Val M where
--   ...
--
-- concreteRun :: [Statement] -> M ()
-- concreteRun = run


-- Instance IsValue AbsVal AbsM where
--   ...
--
-- absRun :: [Statement] -> AbsM ()
-- absRun = run


