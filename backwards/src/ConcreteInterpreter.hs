{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
module ConcreteInterpreter where

import           Data.Concrete.Error
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.Text (Text)

import           GHC.Generics (Generic)

import           Text.Printf

import           Syntax (Expr(..))

type Env = HashMap Text Val
data Val = NumVal Int deriving (Eq,Generic)

eval :: Env -> Expr -> Error String Val
eval env e = case e of
  Var x -> case M.lookup x env of
    Just v  -> return v
    Nothing -> Fail $ printf "variable %s not bound" (show x)
  NumLit n -> return $ NumVal n
  Add e1 e2 -> do
    v1 <- eval env e1
    v2 <- eval env e2
    case (v1,v2) of
      (NumVal n1, NumVal n2) -> return (NumVal (n1 + n2))

instance Show Val where
  show (NumVal iv) = show iv

-- These instances allow to use prettier syntax to construct values.
instance Num Val where
  fromInteger n = NumVal (fromInteger n)
  (+)    = error "unsupported operation"
  (-)    = error "unsupported operation"
  (*)    = error "unsupported operation"
  negate = error "unsupported operation"
  abs    = error "unsupported operation"
  signum = error "unsupported operation"
