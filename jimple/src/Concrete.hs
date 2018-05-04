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
  = VBottom
  | VInt Int
  | VFloat Float
  | VString String
  | VClass String
  | VBool Bool
  | VNull
  | VArray [Val]
  | VObject String (Map String Val) deriving (Eq)

type StaticStore = Map String (Maybe Val)
type DynamicStore = Map String (Maybe Val)
type Store = (StaticStore, DynamicStore)

instance Show Val where
  show VBottom = "⊥"
  show (VInt n) = show n
  show (VFloat f) = show f
  show (VString s) = s
  show (VClass c) = "<" ++ c ++ ">"
  show (VBool b) = show b
  show VNull = "null"
  show (VArray v) = show v
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

defaultValue :: Type -> Val
defaultValue TBoolean = VInt 0
defaultValue TByte = VInt 0
defaultValue TChar = VInt 0
defaultValue TShort = VInt 0
defaultValue TInt = VInt 0
defaultValue TLong = VInt 0
defaultValue TFloat = VFloat 0.0
defaultValue TDouble = VFloat 0.0
defaultValue TNull = VNull
defaultValue (TClass c) = VObject c Map.empty
defaultValue _ = VBottom

defaultArray :: Type -> [Val] -> Val
defaultArray t (VInt d:ds) = VArray (replicate d (defaultArray t ds))
defaultArray _ (_:_) = VBottom
defaultArray t [] = defaultValue t

isPositiveVInt :: Val -> Bool
isPositiveVInt (VInt n) = n > 0
isPositiveVInt _ = False

evalImmediateList :: Arr [Immediate] [Val]
evalImmediateList = proc xs -> case xs of
  (x':xs') -> do
    v <- evalImmediate -< x'
    vs <- evalImmediateList -< xs'
    returnA -< (v:vs)
  [] -> returnA -< []

evalImmediate :: Arr Immediate Val
evalImmediate = proc i -> case i of
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

eval :: Arr Expr Val
eval = proc e -> case e of
  ENew newExpr -> case newExpr of
    NewSimple t -> if isBaseType t
      then returnA -< (defaultValue t)
      else throw -< "Expected a nonvoid base type for new"
    NewArray t i -> if isNonvoidType t
      then do
        v <- evalImmediate -< i
        if isPositiveVInt v
          then returnA -< (defaultArray t [v])
          else throw -< "Expected a positive integer for newarray size"
      else throw -< "Expected a nonvoid type for newarray"
    NewMulti t is -> if isBaseType t
      then do
        vs <- evalImmediateList -< is
        if all isPositiveVInt vs
          then returnA -< (defaultArray t vs)
          else throw -< "Expected positive integers for newmultiarray sizes"
      else throw -< "Expected a nonvoid base type for newmultiarray"
  -- ECast NonvoidType Immediate
  -- EInstanceof Immediate NonvoidType
  -- EInvoke InvokeExpr
  -- EReference Reference
  EBinop i1 op i2 -> do
    v1 <- evalImmediate -< i1
    v2 <- evalImmediate -< i2
    case op of
      -- And ->
      -- Or ->
      -- Xor ->
      Mod -> case (v1, v2) of
        (VInt x1, VInt x2) -> returnA -< (VInt (x1 `mod` x2))
        (VInt x1, VFloat x2) -> returnA -< (VFloat (fromIntegral x1 `mod'` x2))
        (VFloat x1, VInt x2) -> returnA -< (VFloat (x1 `mod'` fromIntegral x2))
        (VFloat x1, VFloat x2) -> returnA -< (VFloat (x1 `mod'` x2))
        (_, _) -> throw -< "Expected two numbers as arguments for mod"
      -- Rem ->
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
        (VFloat f, VInt n) -> returnA -< (VFloat (f / fromIntegral n))
        (VInt n, VFloat f) -> returnA -< (VFloat (fromIntegral n / f))
        (VFloat f1, VFloat f2) -> returnA -< (VFloat (f1 / f2))
        (_, _) -> throw -< "Expected two numbers as arguments for /"
  EUnop op i -> do
    v <- evalImmediate -< i
    case op of
      Lengthof -> case v of
        VArray xs -> returnA -< (VInt (length xs))
        _ -> throw -< "Expected an array as argument for lengthof"
      Neg -> case v of
        VInt n -> returnA -< (VInt (-n))
        VFloat f -> returnA -< (VFloat (-f))
        _ -> throw -< "Expected a number as argument for -"
  EImmediate i -> do
    v <- evalImmediate -< i
    returnA -< v
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

goto :: Arr ([Statement], String) (Maybe Val)
goto = proc (stmts, label) -> case Label label `elemIndex` stmts of
  Just i -> run -< (stmts, i)
  Nothing -> throw -< "Undefined label: " ++ label

matchCases :: Arr ([CaseStatement], Int) String
matchCases = proc (cases, v) -> case cases of
  ((CLConstant n, label): cases') -> if v == n
    then returnA -< label
    else matchCases -< (cases', v)
  ((CLDefault, label): _) -> returnA -< label
  [] -> throw -< "No cases match value " ++ show v

run :: Arr ([Statement], Int) (Maybe Val)
run = proc (stmts, i) -> if i == length stmts
  then returnA -< Nothing
  else case stmts !! i of
    -- Label LabelName
    -- Breakpoint
    -- Entermonitor Immediate
    -- Exitmonitor Immediate
    Tableswitch immediate cases -> do
      v <- evalImmediate -< immediate
      case v of
        VInt x -> do
          label <- matchCases -< (cases, x)
          goto -< (stmts, label)
        _ -> throw -< "Expected an integer as argument for switch"
    Lookupswitch immediate cases -> do
      v <- evalImmediate -< immediate
      case v of
        VInt x -> do
          label <- matchCases -< (cases, x)
          goto -< (stmts, label)
        _ -> throw -< "Expected an integer as argument for switch"
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
        VBool True -> goto -< (stmts, label)
        VBool False -> run -< (stmts, i + 1)
        _ -> throw -< "Expected a boolean expression for if statement"
    -- If Expr GotoStatement
    Goto label -> goto -< (stmts, label)
    -- Nop
    -- Ret (Maybe Immediate)
    Return e -> case e of
      Just immediate -> do
        v <- evalImmediate -< immediate
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
