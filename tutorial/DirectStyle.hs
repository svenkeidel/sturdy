module DirectStyle where

import Data.Map (Map)
import qualified Data.Map as Store

data Expr
  = Var String
  | BoolLit Bool
  | And Expr Expr
  | NumLit Int
  | Add Expr Expr
  | Lt Expr Expr

-- x + x > x
expr = Lt (Add (Var "x") (Var "x")) (Var "x")

data Val = BoolVal Bool | NumVal Int
type Store = Map String Val

-- data Maybe a     = Nothing | Just a
-- data Either er a = Left er | Right a

eval :: Store -> Expr -> Either String Val
eval st e = case e of
  Var x -> case Store.lookup x st of
    Just v -> Right v
    Nothing -> Left "Variable not in scope"
  NumLit n -> Right (NumVal n)
  Add e1 e2 ->
    case (eval st e1,eval st e2) of
      (Right (NumVal n1), Right (NumVal n2)) -> Right (NumVal (n1 + n2))
      (Right _,Right _) -> Left "Expected two numbers as arguments for +"
      (Left er, _) -> Left er
      (_, Left er) -> Left er
  BoolLit b -> Right (BoolVal b)
  And e1 e2 ->
    case (eval st e1,eval st e2) of
      (Right (BoolVal b1), Right (BoolVal b2)) -> Right (BoolVal (b1 && b2))
      (Right _,Right _) -> Left "Expected two booleans as arguments for &&"
      (Left er, _) -> Left er
      (_, Left er) -> Left er
  Lt e1 e2 ->
    case (eval st e1,eval st e2) of
      (Right (NumVal n1), Right (NumVal n2)) -> Right (BoolVal (n1 < n2))
      (Right _,Right _) -> Left "Expected two numbers as arguments for +"
      (Left er, _) -> Left er
      (_, Left er) -> Left er

data Statement
  = Assign String Expr               -- x := y + z
  | If Expr [Statement] [Statement]  -- if(x < y) {x:=1} else {y:=2}
  | While Expr [Statement]

run :: Store -> [Statement] -> Either String Store
run st stmts = case stmts of
  (Assign x e : rest) ->
    case eval st e of
      Right v -> run (Store.insert x v st) rest
      Left er -> Left er
  (If cond ifBranch elseBranch : rest) ->
    case eval st cond of
      Right (BoolVal True) -> case run st ifBranch of
        Right st' -> run st' rest
        Left er -> Left er
      Right (BoolVal False) -> case run st elseBranch of
        Right st' -> run st' rest
        Left er -> Left er
      Right (NumVal _) -> Left "Expected a boolean expression as condition for an if"
      Left er -> Left er
  (While cond body : rest) ->
    run st (If cond (body ++ [While cond body]) [] : rest)
  [] ->
    Right st

