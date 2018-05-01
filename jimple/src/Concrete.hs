{-# LANGUAGE Arrows #-}
module Concrete where

import           Control.Category
import           Control.Arrow

import           Data.Map (Map)
import qualified Data.Map as Store

import           Syntax

data Val
  = VInt Int
  | VFloat Float
  | VString String
  | VClass String
  | VNull
  | VArray Val
  | VFixedArray Val Int deriving (Eq)

type Store = Map String Val

instance Show Val where
  show (VInt n) = show n
  show (VFloat f) = show f
  show (VString s) = s
  show (VClass c) = "<" ++ c ++ ">"
  show (VNull) = "null"
  show (VArray v) = (show v) ++ "[]"
  show (VFixedArray v l) = (show v) ++ "[" ++ (show l) ++ "]"

newtype Arr x y = Arr {
  runArr :: x -> Store -> Either String (Store, y)
}

eval :: Arr Expr Val
eval = proc e -> case e of
  -- ENew newExpr -> do
  --   case newExpr of
  --     NewSimple t ->
  --     NewArray t d ->
  --     NewMulti t ds ->
  -- ECast NonvoidType Immediate
  -- EInstanceof Immediate NonvoidType
  -- EInvoke InvokeExpr
  -- EReference Reference
  EBinop i1 op i2 -> do
    v1 <- eval -< EImmediate i1
    v2 <- eval -< EImmediate i2
    case op of
      -- And ->
      -- Or ->
      -- Xor ->
      -- Mod ->
      -- Cmp ->
      -- Cmpg ->
      -- Cmpl ->
      -- Cmpeq ->
      -- Cmpne ->
      -- Cmpgt ->
      -- Cmpge ->
      -- Cmplt ->
      -- Cmple ->
      -- Shl ->
      -- Shr ->
      -- Ushr ->
      Plus ->
        case (v1, v2) of
          (VInt n1, VInt n2) -> returnA -< (VInt (n1 + n2))
          (VFloat f, VInt n) -> returnA -< (VFloat (f + (fromIntegral n)))
          (VInt n, VFloat f) -> returnA -< (VFloat ((fromIntegral n) + f))
          (VFloat f1, VFloat f2) -> returnA -< (VFloat (f1 + f2))
          (_,_) -> throw -< "Expected two numbers as arguments for +"
      Minus ->
        case (v1, v2) of
          (VInt n1, VInt n2) -> returnA -< (VInt (n1 - n2))
          (VFloat f, VInt n) -> returnA -< (VFloat (f - (fromIntegral n)))
          (VInt n, VFloat f) -> returnA -< (VFloat ((fromIntegral n) - f))
          (VFloat f1, VFloat f2) -> returnA -< (VFloat (f1 - f2))
          (_,_) -> throw -< "Expected two numbers as arguments for -"
      Mult ->
        case (v1, v2) of
          (VInt n1, VInt n2) -> returnA -< (VInt (n1 * n2))
          (VFloat f, VInt n) -> returnA -< (VFloat (f * (fromIntegral n)))
          (VInt n, VFloat f) -> returnA -< (VFloat ((fromIntegral n) * f))
          (VFloat f1, VFloat f2) -> returnA -< (VFloat (f1 * f2))
          (_,_) -> throw -< "Expected two numbers as arguments for *"
      Div ->
        case (v1, v2) of
          (_, VInt 0) -> throw -< "Cannot divide by zero"
          (_, VFloat 0.0) -> throw -< "Cannot divide by zero"
          (VInt n1, VInt n2) -> returnA -< (VFloat ((fromIntegral n1) / (fromIntegral n2)))
          (VFloat f, VInt n) -> returnA -< (VFloat (f / (fromIntegral n)))
          (VInt n, VFloat f) -> returnA -< (VFloat ((fromIntegral n) / f))
          (VFloat f1, VFloat f2) -> returnA -< (VFloat (f1 / f2))
          (_,_) -> throw -< "Expected two numbers as arguments for /"
  -- EUnop Unop Immediate
  EImmediate i ->
    case i of
      ILocalName x -> do
        st <- get -< ()
        case Store.lookup x st of
          Just v -> returnA -< v
          Nothing -> throw -< "Variable not in scope"
      IInt n -> returnA -< (VInt n)
      IFloat f -> returnA -< (VFloat f)
      IString s -> returnA -< (VString s)
      IClass c -> returnA -< (VClass c)
      INull -> returnA -< VNull
  _ -> throw -< "Undefined expression"

eval' :: Store -> Expr -> Either String (Store, Val)
eval' store expr = runArr eval expr store

get :: Arr () Store
get = Arr (\() st -> Right (st,st))

put :: Arr Store ()
put = Arr (\st _ -> Right (st,()))

throw :: Arr String a
throw = Arr (\er _ -> Left er)

run :: Arr [Statement] ()
run = proc stmts -> case stmts of
  (_ : rest) -> do
    run -< rest
  [] ->
    returnA -< ()

instance Category Arr where
  id = Arr (\x st -> Right (st,x))
  Arr f . Arr g = Arr $ \x st -> case g x st of
    Left er -> Left er
    Right (st',y) -> f y st'

instance Arrow Arr where
  arr f = Arr (\x st -> Right (st,f x))
  first (Arr f) = Arr $ \(x,y) st -> case f x st of
    Left er -> Left er
    Right (st',z) -> Right (st',(z,y))

instance ArrowChoice Arr where
  left (Arr f) = Arr $ \e st -> case e of
    Left x -> case f x st of
      Left er -> Left er
      Right (st',y) -> Right (st',Left y)
    Right z -> Right (st,Right z)
