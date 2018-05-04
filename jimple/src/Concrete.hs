{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
module Concrete where

import           Control.Category
import           Control.Arrow

import           Data.Fixed
import           Data.List

import           Data.Map (Map)
import qualified Data.Map as Map

import           Syntax

data Val
  = VInt Int
  | VFloat Float
  | VString String
  | VClass String
  | VBool Bool
  | VNull
  | VArray Type [Val]
  | VFixedArray Type [Val] Int
  | VObject Type (Map String Val) deriving (Eq)

type StaticStore = Map String (Maybe Val)
type DynamicStore = Map String (Maybe Val)
type Store = (StaticStore, DynamicStore)

instance Show Val where
  show (VInt n) = show n
  show (VFloat f) = show f
  show (VString s) = s
  show (VClass c) = "<" ++ c ++ ">"
  show (VBool b) = show b
  show (VNull) = "null"
  show (VArray v) = (show v) ++ "[]"
  show (VFixedArray v l) = (show v) ++ "[" ++ (show l) ++ "]"
  show (VObject t m) = show t ++ "(" ++ show m ++ ")"

newtype Arr x y = Arr {
  runArr :: x -> Store -> Either String (Store, y)
}

numToNum :: (forall a. Num a => a -> a -> a) -> Val -> Val -> Maybe Val
numToNum op v1 v2 = case (v1, v2) of
  (VInt n1, VInt n2) -> Just (VInt (op n1 n2))
  (VFloat f, VInt n) -> Just (VFloat (op f (fromIntegral n)))
  (VInt n, VFloat f) -> Just (VFloat (op (fromIntegral n) f))
  (VFloat f1, VFloat f2) -> Just (VFloat (op f1 f2))
  (_, _) -> Nothing

numToBool :: (forall a. Ord a => a -> a -> Bool) -> Val -> Val -> Maybe Val
numToBool op v1 v2 = case (v1, v2) of
  (VInt n1, VInt n2) -> Just (VBool (op n1 n2))
  (VFloat f, VInt n) -> Just (VBool (op f (fromIntegral n)))
  (VInt n, VFloat f) -> Just (VBool (op (fromIntegral n) f))
  (VFloat f1, VFloat f2) -> Just (VBool (op f1 f2))
  (_, _) -> Nothing

eval :: Arr Expr Val
eval = proc e -> case e of
  ENew newExpr -> do
    case newExpr of
      NewSimple t -> VObject (t, 0) Map.empty
      NewArray t d -> VObject (t, d) Map.empty
      NewMulti t ds ->
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
      Mod -> case (v1, v2) of
        (VInt x1, VInt x2) -> returnA -< (VInt (x1 `mod` x2))
        (VInt x1, VFloat x2) -> returnA -< (VFloat ((fromIntegral x1) `mod'` x2))
        (VFloat x1, VInt x2) -> returnA -< (VFloat (x1 `mod'` (fromIntegral x2)))
        (VFloat x1, VFloat x2) -> returnA -< (VFloat (x1 `mod'` x2))
        (_, _) -> throw -< "Expected two numbers as arguments for mod"
      -- Cmp ->
      -- Cmpg ->
      -- Cmpl ->
      Cmpeq -> returnA -< (VBool (v1 == v2))
      Cmpne -> returnA -< (VBool (v1 /= v2))
      Cmpgt -> case numToBool (>) v1 v2 of
        Just v -> returnA -< v
        Nothing -> throw -< "Expected two numbers as arguments for >"
      Cmpge -> case numToBool (>=) v1 v2 of
        Just v -> returnA -< v
        Nothing -> throw -< "Expected two numbers as arguments for >="
      Cmplt -> case numToBool (<) v1 v2 of
        Just v -> returnA -< v
        Nothing -> throw -< "Expected two numbers as arguments for <"
      Cmple -> case numToBool (<=) v1 v2 of
        Just v -> returnA -< v
        Nothing -> throw -< "Expected two numbers as arguments for <="
      -- Shl ->
      -- Shr ->
      -- Ushr ->
      Plus -> case numToNum (+) v1 v2 of
        Just v -> returnA -< v
        Nothing -> throw -< "Expected two numbers as arguments for +"
      Minus -> case numToNum (-) v1 v2 of
        Just v -> returnA -< v
        Nothing -> throw -< "Expected two numbers as arguments for -"
      Mult -> case numToNum (*) v1 v2 of
        Just v -> returnA -< v
        Nothing -> throw -< "Expected two numbers as arguments for *"
      Div -> case (v1, v2) of
        (_, VInt 0) -> throw -< "Cannot divide by zero"
        (_, VFloat 0.0) -> throw -< "Cannot divide by zero"
        (VInt n1, VInt n2) -> returnA -< (VInt (n1 `div` n2))
        (VFloat f, VInt n) -> returnA -< (VFloat (f / (fromIntegral n)))
        (VInt n, VFloat f) -> returnA -< (VFloat ((fromIntegral n) / f))
        (VFloat f1, VFloat f2) -> returnA -< (VFloat (f1 / f2))
        (_, _) -> throw -< "Expected two numbers as arguments for /"
  EUnop op i -> do
    v <- eval -< EImmediate i
    case op of
      Lengthof -> case v of
        VArray xs -> returnA -< (VInt (length xs))
        VFixedArray _ l -> returnA -< (VInt l)
        _ -> throw -< "Expected an array as argument for lengthof"
      Neg -> case v of
        VInt n -> returnA -< (VInt (-n))
        VFloat f -> returnA -< (VFloat (-f))
        _ -> throw -< "Expected a number as argument for -"
  EImmediate i -> case i of
    ILocalName x -> do
      st <- getDynamic -< ()
      case Map.lookup x st of
        Just v -> case v of
          Just a -> returnA -< a
          Nothing -> throw -< "Variable not initialized"
        Nothing -> throw -< "Variable not in scope"
    IInt n -> returnA -< (VInt n)
    IFloat f -> returnA -< (VFloat f)
    IString s -> returnA -< (VString s)
    IClass c -> returnA -< (VClass c)
    INull -> returnA -< VNull
  _ -> throw -< "Undefined expression"

eval' :: Store -> Expr -> Either String Val
eval' store expr = right snd (runArr eval expr store)

get :: Arr () Store
get = Arr (\() st -> Right (st,st))

getDynamic :: Arr () DynamicStore
getDynamic = Arr (\() st@(_, dynamic) -> Right (st, dynamic))

put :: Arr Store ()
put = Arr (\st _ -> Right (st,()))

throw :: Arr String a
throw = Arr (\er _ -> Left er)

run :: Arr ([Statement], Int) (Maybe Val)
run = proc (stmts, i) -> if i == length stmts
  then returnA -< Nothing
  else case stmts !! i of
    -- Label LabelName
    -- Breakpoint
    -- Entermonitor Immediate
    -- Exitmonitor Immediate
    -- Tableswitch Immediate [CaseStatement]
    -- Lookupswitch Immediate [CaseStatement]
    -- Identity LocalName AtIdentifier Type
    -- IdentityNoType LocalName AtIdentifier
    Assign var e -> do
      v <- eval -< e
      (static, dynamic) <- get -< ()
      case var of
        VLocal localName -> case Map.lookup localName dynamic of
          Just _ -> put -< (static, Map.insert localName (Just v) dynamic)
          Nothing -> throw -< "Variable not declared"
        VReference _ -> throw -< "undefined yet"
      run -< (stmts, i + 1)
    If e label -> do
      v <- eval -< e
      case v of
        VBool True -> do
          case ((Label label) `elemIndex` stmts) of
            Just j -> run -< (stmts, j)
            Nothing -> throw -< "Undefined label: " ++ label
        VBool False -> run -< (stmts, i + 1)
        _ -> throw -< "Expected a boolean expression for if statement"
    -- If Expr GotoStatement
    Goto label -> case ((Label label) `elemIndex` stmts) of
      Just j -> run -< (stmts, j)
      Nothing -> throw -< "Undefined label: " ++ label
    -- Nop
    -- Ret (Maybe Immediate)
    Return e -> case e of
      Just immediate -> do
        v <- eval -< EImmediate immediate
        returnA -< Just v
      Nothing -> returnA -< Nothing
    -- Throw Immediate
    -- Invoke Expr
    _ -> run -< (stmts, i + 1)

run' :: Store -> [Statement] -> Either String (Store, Maybe Val)
run' st stmts = runArr run (stmts, 0) st

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
