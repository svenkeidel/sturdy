{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module ConcreteInterpreter where

import           Control.DeepSeq

import           Data.Concrete.Error
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.Text (Text)

import           GHC.Generics (Generic)

import           Text.Printf

import           Syntax (Expr(..))

type Env = HashMap Text Val
data Val = NumVal Int | BoolVal Bool
  deriving (Eq,Generic,NFData)

eval :: Expr -> Env -> Error String Val
eval e env = case e of
  Var x -> case M.lookup x env of
    Just v  -> return v
    Nothing -> fail $ printf "variable %s not bound" (show x)
  NumLit n -> return $ NumVal n
  Add e1 e2 -> do
    v1 <- eval e1 env
    v2 <- eval e2 env
    case (v1,v2) of
      (NumVal n1, NumVal n2) -> return (NumVal (n1 + n2))
      (_, _)                 ->
        fail $ printf "expected two numbers as arguments for addition, but got %s, %s" (show v1) (show v2)
  BoolLit b -> return $ BoolVal b
  If e1 e2 e3 -> do
    v <- eval e1 env
    case v of
      BoolVal True  -> eval e2 env
      BoolVal False -> eval e3 env
      _             ->
        fail $ printf "expected a boolean as condition of an if expression, but got %s" (show v)
instance Show Val where
  show (NumVal n) = show n
  show (BoolVal b) = show b

-- These instances allow to use prettier syntax to construct values.
instance Num Val where
  fromInteger n = NumVal (fromInteger n)
  (+)    = error "unsupported operation"
  (-)    = error "unsupported operation"
  (*)    = error "unsupported operation"
  negate = error "unsupported operation"
  abs    = error "unsupported operation"
  signum = error "unsupported operation"
