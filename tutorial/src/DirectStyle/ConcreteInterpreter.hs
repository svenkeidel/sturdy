-- | This is an interpreter for the While language in direct-style. All
-- effects of the language such as store-passing or error propagation
-- are handled explicitly. This style is not only hard to understand,
-- it also makes it hard to change or extend the language. For example,
-- if we were to add a new effect to the language, we would need to
-- change the entire interpreter.
module DirectStyle.ConcreteInterpreter where

import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Label
import           Data.Maybe

import           Syntax

data Val = BoolVal Bool | NumVal Int
type Addr = Label
type Env = Map String Addr
type Store = Map Addr Val

-- data Maybe a     = Nothing | Just a
-- data Either er a = Left er | Right a
eval :: Env -> Store -> Expr -> Either String Val
eval env store e = case e of
  Var x _ -> case M.lookup x env of
    Just addr -> case M.lookup addr store of
      Just val -> Right val
      Nothing -> Left "Variable not in scope"
    Nothing -> Left "Variable not in scope"
  NumLit n _ -> Right (NumVal n)
  Add e1 e2 _ ->
    case (eval env store e1,eval env store e2) of
      (Right (NumVal n1), Right (NumVal n2)) -> Right (NumVal (n1 + n2))
      (Right _,Right _) -> Left "Expected two numbers as arguments for +"
      (Left er, _) -> Left er
      (_, Left er) -> Left er
  BoolLit b _ -> Right (BoolVal b)
  And e1 e2 _ ->
    case (eval env store e1,eval env store e2) of
      (Right (BoolVal b1), Right (BoolVal b2)) -> Right (BoolVal (b1 && b2))
      (Right _,Right _) -> Left "Expected two booleans as arguments for &&"
      (Left er, _) -> Left er
      (_, Left er) -> Left er
  Lt e1 e2 _ ->
    case (eval env store e1,eval env store e2) of
      (Right (NumVal n1), Right (NumVal n2)) -> Right (BoolVal (n1 < n2))
      (Right _,Right _) -> Left "Expected two numbers as arguments for <"
      (Left er, _) -> Left er
      (_, Left er) -> Left er

run :: Env -> Store -> [Statement] -> Either String Store
run env store stmts = case stmts of
  (Assign x e l : rest) ->
    case eval env store e of
      Right v -> let addr = fromMaybe l (M.lookup x env)
                 in run (M.insert x addr env) (M.insert addr v store) rest
      Left er -> Left er
  (If cond ifBranch elseBranch _ : rest) ->
    case eval env store cond of
      Right (BoolVal True) -> case run env store ifBranch of
        Right store' -> run env store' rest
        Left er -> Left er
      Right (BoolVal False) -> case run env store elseBranch of
        Right store' -> run env store' rest
        Left er -> Left er
      Right (NumVal _) -> Left "Expected a boolean expression as condition for an if"
      Left er -> Left er
  (While cond body l : rest) ->
    run env store (If cond (body ++ [While cond body l]) [] l : rest)
  [] ->
    Right store

