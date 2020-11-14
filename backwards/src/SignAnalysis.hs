{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
module SignAnalysis where

import           Control.DeepSeq

import           Data.Text (Text)
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.Text.Prettyprint.Doc

import           Data.Order
import           Data.Abstract.Error (Error(..))
import           Data.Abstract.Interval (Interval(..))
import           Data.Abstract.DiscretePowerset (Pow)
import           Data.Abstract.FreeCocompletion

import qualified Data.Abstract.Boolean as Abs
import qualified Data.Abstract.Sign as AbsS
import qualified Type as AbsT


import           GHC.Generics(Generic)
import           Text.Printf

import           Syntax (Expr(..), Type(..))

type Env = HashMap Text (FreeCocompletion Val)

data Val = Sign AbsS.Sign | BoolVal Abs.Bool | TypeVal AbsT.Type | Top
  deriving (Eq, Generic, NFData)

-- | 'eval' is an interpreter that approximates the forward collecting semantics.
eval :: Expr -> Env -> Error (Pow String) Val
eval e env = case e of
  Var x -> case M.lookup x env of
    Just (Upper v) -> return v
    Just Bottom    -> fail $ printf "variable %s was bottom" (show x)
    Nothing        -> fail $ printf "variable %s not bound" (show x)
  NumLit n
    | n > 0 -> return $ Sign AbsS.Positive
    | n < 0 -> return $ Sign AbsS.Negative
    | otherwise -> return $ Sign AbsS.Zero
  BoolLit True  -> return $ BoolVal Abs.True
  BoolLit False -> return $ BoolVal Abs.False
  TypeLit Number -> return $ TypeVal AbsT.Number
  TypeLit Boolean -> return $ TypeVal AbsT.Boolean
  Add e1 e2 -> do
    v1 <- eval e1 env
    v2 <- eval e2 env
    case (v1, v2) of
      (Sign n, Sign m)                      -> return $ Sign (n + m)
      (_, _)                                ->
        fail $ printf "expected two numbers as arguments for addition, but got %s, %s" (show v1) (show v2)
  Sub e1 e2 -> do
    v1 <- eval e1 env
    v2 <- eval e2 env
    case (v1, v2) of
      (Sign n, Sign m)                      -> return $ Sign (n - m)
      (_, _)                                ->
        fail $ printf "expected two numbers as arguments for subtraction, but got %s, %s" (show v1) (show v2)
  Mul e1 e2 -> do
    v1 <- eval e1 env
    v2 <- eval e2 env
    case (v1, v2) of
      (Sign n, Sign m)                      -> return $ Sign (n * m)
      (_, _)                                ->
        fail $ printf "expected two numbers as arguments for subtraction, but got %s, %s" (show v1) (show v2)
  And e1 e2 -> do
    v1 <- eval e1 env
    v2 <- eval e2 env
    case (v1, v2) of
      (BoolVal n, BoolVal m)                -> return $ BoolVal (cand n m)
      (_, _)                                ->
        fail $ printf "expected two numbers as arguments for addition, but got %s, %s" (show v1) (show v2)
  Or e1 e2 -> do
    v1 <- eval e1 env
    v2 <- eval e2 env
    case (v1, v2) of
      (BoolVal n, BoolVal m)                -> return $ BoolVal (cor n m)
      (_, _)                                ->
        fail $ printf "expected two Abs Bools as arguments for Or, but got %s, %s" (show v1) (show v2)
  Not e1 -> do
    v1 <- eval e1 env
    case v1 of
      BoolVal n                             -> return $ BoolVal (cnot n)
      _                                     ->
        fail $ printf "expected Abs Bool as argument for Not, but got %s, " (show v1)
  Equals e1 e2 -> do
    v1 <- eval e1 env
    v2 <- eval e2 env
    if v1 == v2 then return $ BoolVal Abs.True else
      if v1 ⊑ v2 || v2 ⊑ v1 then return $ BoolVal Abs.Top else
        return $ BoolVal Abs.False
  Leq e1 e2 -> do
    v1 <- eval e1 env
    v2 <- eval e2 env
    case (v1, v2) of
      (Sign a, Sign b) -> return $ BoolVal (cLeq a b)
      (_, _)           ->
        fail $ printf "wrong arguments got %s, %s" (show v1) (show v2)

  TypeOf e1 e2 -> do
    v1 <- eval e1 env
    v2 <- eval e2 env
    case (v1, v2) of
      (Sign _, TypeVal AbsT.Number)         -> return $ BoolVal Abs.True
      (BoolVal _, TypeVal AbsT.Boolean)     -> return $ BoolVal Abs.True
      (Sign _, _)                           -> return $ BoolVal Abs.False
      (BoolVal _, _)                        -> return $ BoolVal Abs.False
      (_, _)                                ->
        fail $ printf "wrong arguments got %s, %s" (show v1) (show v2)
  If e1 e2 e3 -> do
    v <- eval e1 env
    case v of
      BoolVal Abs.True  ->
        case evalᴮ e1 env (BoolVal Abs.True) of
          Upper refinedEnv -> eval e2 refinedEnv
          Bottom           -> fail $ printf "True branch cannot occur."
      BoolVal Abs.False ->
        case evalᴮ e1 env (BoolVal Abs.False) of
          Upper refinedEnv -> eval e2 refinedEnv
          Bottom           -> fail $ printf "False branch cannot occur."
      BoolVal Abs.Top ->
        case (evalᴮ e1 env (BoolVal Abs.True), evalᴮ e1 env (BoolVal Abs.False)) of
          (Upper refinedEnvTrue, Upper refinedEnvFalse) -> eval e2 refinedEnvTrue ⊔ eval e3 refinedEnvFalse
          (Upper refinedEnvTrue, Bottom)                -> eval e2 refinedEnvTrue
          (Bottom,               Upper refinedEnvFalse) -> eval e3 refinedEnvFalse
          (Bottom,               Bottom)                -> fail $ printf "Both branches cannot occur."
      _ -> fail $ printf "expected a boolean as condition of an if expression, but got %s" (show v)
  where
    cand b1 b2 = case (b1,b2) of
      (Abs.True, Abs.True)        -> Abs.True
      (Abs.False, _)              -> Abs.False
      (_, Abs.False)              -> Abs.False
      (_,_)                       -> Abs.Top
    cor b1 b2 = case (b1,b2) of
      (Abs.True, _)               -> Abs.True
      (_, Abs.True)               -> Abs.True
      (Abs.False, Abs.False)      -> Abs.False
      (_,_)                       -> Abs.Top
    cnot b1 = case b1 of
      Abs.True                    -> Abs.False
      Abs.False                   -> Abs.True
      Abs.Top                     -> Abs.Top
    cLeq n1 n2 = case (n1, n2) of
      (AbsS.Top, _)               -> Abs.Top
      (_, AbsS.Top)               -> Abs.Top
      (AbsS.Negative, AbsS.Negative)        -> Abs.Top
      (AbsS.Negative, _)               -> Abs.True
      (_, AbsS.Negative)               -> Abs.False
      (AbsS.Zero, _)              -> Abs.True
      (AbsS.Positive, AbsS.Positive)        -> Abs.True
      (AbsS.Positive, AbsS.Zero)       -> Abs.False

-- | 'evalᴮ' is an interpreter that approximates the backward collecting semantics.
evalᴮ :: Expr -> Env -> Val -> FreeCocompletion Env
evalᴮ e env res = case e of
  Var x -> return $ M.insert x (Upper res) env
  TypeLit n
    | n ∈ res   -> return env
    | otherwise -> bottom
  NumLit n
    | n ∈ res   -> return env
    | otherwise -> bottom
  Add e1 e2 ->
    case (eval e1 env, eval e2 env, res) of
      (Success (Sign n), Success (Sign m), Sign r) -> do
        (n', m') <- addᴮ n m r
        evalᴮ e1 env (Sign n') ⊓ evalᴮ e2 env (Sign m')
      _ -> bottom
  Sub e1 e2 ->
    case (eval e1 env, eval e2 env, res) of
      (Success (Sign n), Success (Sign m), Sign r) -> do
        (n', m') <- subᴮ n m r
        evalᴮ e1 env (Sign n') ⊓ evalᴮ e2 env (Sign m')
      _ -> bottom
  Mul e1 e2 ->
    case (eval e1 env, eval e2 env, res) of
      (Success (Sign n), Success (Sign m), Sign r) -> do
        (n', m') <- mulᴮ n m r
        evalᴮ e1 env (Sign n') ⊓ evalᴮ e2 env (Sign m')
      _ -> bottom
  BoolLit n
    | n ∈ res   -> return env
    | otherwise -> bottom
  If e1 e2 e3 ->
    case (eval e1 env) of
      (Success b@(BoolVal Abs.True)) -> do
        env2 <- evalᴮ e2 env res
        evalᴮ e1 env2 b
      (Success b@(BoolVal Abs.False)) -> do
        env3 <- evalᴮ e3 env res
        evalᴮ e1 env3 b
      (Success b@(BoolVal Abs.Top)) -> do
        env23 <- evalᴮ e2 env res ⊓ evalᴮ e3 env res
        evalᴮ e1 env23 b
      _ -> bottom
  And e1 e2 ->
    case (eval e1 env, eval e2 env, res) of
      (Success (BoolVal n), Success (BoolVal m), BoolVal r) -> do
        (n', m') <- andᴮ n m r
        evalᴮ e1 env (BoolVal n') ⊓ evalᴮ e2 env (BoolVal m')
      _ -> bottom
  Or  e1 e2 ->
    case (eval e1 env, eval e2 env, res) of
      (Success (BoolVal n), Success (BoolVal m), BoolVal r) -> do
        (n', m') <- orᴮ n m r
        evalᴮ e1 env (BoolVal n') ⊓ evalᴮ e2 env (BoolVal m')
      _ -> bottom
  Not e1 ->
    case (eval e1 env, res) of
      (Success (BoolVal n), BoolVal r) -> do
        n' <- notᴮ n r
        evalᴮ e1 env (BoolVal n')
      _ -> bottom
  Equals e1 e2 ->
    case (eval e1 env, eval e2 env, res) of
      (Success n, Success m, BoolVal r) -> do
        (n', m') <- eqᴮ n m r
        evalᴮ e1 env n' ⊓ evalᴮ e2 env m'
      _ -> bottom
  Leq e1 e2 ->
    case (eval e1 env, eval e2 env, res) of
      (Success (Sign n), Success (Sign m), BoolVal r) -> do
        (n', m') <- leqᴮ n m r
        evalᴮ e1 env (Sign n') ⊓ evalᴮ e2 env (Sign m')
      _ -> bottom
  TypeOf e1 e2 ->
    case (eval e1 env, eval e2 env, res) of
      (Success x, Success (TypeVal m), BoolVal r) -> do
        (n', m') <- typeOfᴮ x (TypeVal m) (BoolVal r)
        evalᴮ e1 env n' ⊓ evalᴮ e2 env m'
      _ -> bottom
  where

    -- add :: (Sign,Sign) -> Sign
    -- addᴮ :: ((Sign,Sign),Sign) -> FreeCocompletion (Sign,Sign)
    addᴮ :: Sign -> Sign -> Sign -> FreeCocompletion (Sign,Sign)
    addᴮ i j x = case (i, j, x) of
      (AbsS.Negative,  AbsS.Top,       AbsS.Zero)     -> return (AbsS.Negative,  AbsS.Positive)
      (AbsS.Negative,  AbsS.Top,       AbsS.Positive) -> return (AbsS.Negative,  AbsS.Positive)
      (AbsS.Zero,      AbsS.Top,       AbsS.Negative) -> return (AbsS.Zero, AbsS.Negative)
      (AbsS.Zero,      AbsS.Top,       AbsS.Zero)     -> return (AbsS.Zero, AbsS.Zero)
      (AbsS.Zero,      AbsS.Top,       AbsS.Positive) -> return (AbsS.Zero, AbsS.Positive)
      (AbsS.Positive,  AbsS.Top,       AbsS.Negative) -> return (AbsS.Positive,  AbsS.Negative)
      (AbsS.Positive,  AbsS.Top,       AbsS.Zero)     -> return (AbsS.Positive,  AbsS.Negative)
      (AbsS.Top,       AbsS.Negative,  AbsS.Zero)     -> return (AbsS.Positive,  AbsS.Negative)
      (AbsS.Top,       AbsS.Negative,  AbsS.Positive) -> return (AbsS.Positive,  AbsS.Negative)
      (AbsS.Top,       AbsS.Zero,      AbsS.Negative) -> return (AbsS.Negative,  AbsS.Zero)
      (AbsS.Top,       AbsS.Zero,      AbsS.Zero)     -> return (AbsS.Zero, AbsS.Zero)
      (AbsS.Top,       AbsS.Zero,      AbsS.Positive) -> return (AbsS.Positive,  AbsS.Zero)
      (AbsS.Top,       AbsS.Positive,  AbsS.Negative) -> return (AbsS.Negative,  AbsS.Positive)
      (AbsS.Top,       AbsS.Positive,  AbsS.Zero)     -> return (AbsS.Negative,  AbsS.Positive)
      (AbsS.Negative,  AbsS.Negative,  AbsS.Zero)     -> bottom
      (AbsS.Negative,  AbsS.Negative,  AbsS.Positive) -> bottom
      (AbsS.Negative,  AbsS.Zero,      AbsS.Zero)     -> bottom
      (AbsS.Zero,      AbsS.Negative,  AbsS.Zero)     -> bottom
      (AbsS.Zero,      AbsS.Negative,  AbsS.Positive) -> bottom
      (AbsS.Zero,      AbsS.Zero,      AbsS.Negative) -> bottom
      (AbsS.Zero,      AbsS.Zero,      AbsS.Positive) -> bottom
      (AbsS.Zero,      AbsS.Positive,  AbsS.Negative) -> bottom
      (AbsS.Zero,      AbsS.Positive,  AbsS.Zero)     -> bottom
      (AbsS.Positive,  AbsS.Zero,      AbsS.Negative) -> bottom
      (AbsS.Positive,  AbsS.Zero,      AbsS.Zero)     -> bottom
      (AbsS.Positive,  AbsS.Positive,  AbsS.Negative) -> bottom
      (AbsS.Positive,  AbsS.Positive,  AbsS.Zero)     -> bottom
      (_, _, _)                                       -> return (i, j)

    subᴮ i j x = addᴮ i (negate j) x

    mulᴮ i j x = case (i, j, x) of
      (AbsS.Negative,  AbsS.Top,       AbsS.Negative) -> return (AbsS.Negative,  AbsS.Positive)
      (AbsS.Negative,  AbsS.Top,       AbsS.Zero)     -> return (AbsS.Negative,  AbsS.Zero)
      (AbsS.Negative,  AbsS.Top,       AbsS.Positive) -> return (AbsS.Negative,  AbsS.Negative)
      (AbsS.Positive,  AbsS.Top,       AbsS.Negative) -> return (AbsS.Positive,  AbsS.Negative)
      (AbsS.Positive,  AbsS.Top,       AbsS.Zero)     -> return (AbsS.Positive,  AbsS.Zero)
      (AbsS.Positive,  AbsS.Top,       AbsS.Positive) -> return (AbsS.Positive,  AbsS.Positive)
      (AbsS.Top,       AbsS.Negative,  AbsS.Negative) -> return (AbsS.Positive,  AbsS.Negative)
      (AbsS.Top,       AbsS.Negative,  AbsS.Zero)     -> return (AbsS.Zero,      AbsS.Negative)
      (AbsS.Top,       AbsS.Negative,  AbsS.Positive) -> return (AbsS.Negative,  AbsS.Negative)
      (AbsS.Top,       AbsS.Positive,  AbsS.Negative) -> return (AbsS.Negative,  AbsS.Positive)
      (AbsS.Top,       AbsS.Positive,  AbsS.Zero)     -> return (AbsS.Zero,      AbsS.Positive)
      (AbsS.Top,       AbsS.Positive,  AbsS.Positive) -> return (AbsS.Positive,  AbsS.Positive)
      (AbsS.Negative,  AbsS.Negative,  AbsS.Negative) -> bottom
      (AbsS.Negative,  AbsS.Negative,  AbsS.Zero)     -> bottom
      (AbsS.Negative,  AbsS.Zero,      AbsS.Negative) -> bottom
      (AbsS.Negative,  AbsS.Zero,      AbsS.Positive) -> bottom
      (AbsS.Negative,  AbsS.Positive,  AbsS.Zero)     -> bottom
      (AbsS.Negative,  AbsS.Positive,  AbsS.Positive) -> bottom
      (AbsS.Zero,      AbsS.Negative,  AbsS.Zero)     -> bottom
      (AbsS.Zero,      AbsS.Top,       AbsS.Negative) -> bottom
      (AbsS.Zero,      AbsS.Top,       AbsS.Positive) -> bottom
      (AbsS.Positive,  AbsS.Negative,  AbsS.Zero)     -> bottom
      (AbsS.Positive,  AbsS.Negative,  AbsS.Positive) -> bottom
      (AbsS.Positive,  AbsS.Zero,      AbsS.Negative) -> bottom
      (AbsS.Positive,  AbsS.Zero,      AbsS.Positive) -> bottom
      (AbsS.Positive,  AbsS.Positive,  AbsS.Negative) -> bottom
      (AbsS.Positive,  AbsS.Positive,  AbsS.Zero)     -> bottom
      (AbsS.Top,       AbsS.Zero,      AbsS.Negative) -> bottom
      (_, _, _)                                       -> return (i, j)

    andᴮ i j x = case (i, j, x) of
      (Abs.True,  Abs.Top,   Abs.False) -> return (Abs.True,  Abs.False)
      (Abs.True,  Abs.Top,   Abs.True)  -> return (Abs.True,  Abs.True)
      (Abs.Top,   Abs.True,  Abs.False) -> return (Abs.False, Abs.True)
      (Abs.Top,   Abs.True,  Abs.True)  -> return (Abs.True,  Abs.True)
      (Abs.True,  Abs.True,  Abs.False) -> bottom
      (Abs.True,  Abs.False, Abs.True)  -> bottom
      (Abs.False, Abs.True,  Abs.True)  -> bottom
      (Abs.False, Abs.False, Abs.True)  -> bottom
      (Abs.False, Abs.Top,   Abs.True)  -> bottom
      (Abs.Top,   Abs.False, Abs.True)  -> bottom
      (_, _, _)                         -> return (i, j)

    orᴮ i j x = case (i, j, x) of
      (Abs.False, Abs.Top,   Abs.False) -> return (Abs.False, Abs.False)
      (Abs.False, Abs.Top,   Abs.True)  -> return (Abs.False, Abs.True)
      (Abs.Top,   Abs.False, Abs.False) -> return (Abs.False, Abs.False)
      (Abs.Top,   Abs.True,  Abs.True)  -> return (Abs.True,  Abs.False)
      (Abs.True,  Abs.True,  Abs.False) -> bottom
      (Abs.True,  Abs.False, Abs.False) -> bottom
      (Abs.True,  Abs.False, Abs.Top)   -> bottom
      (Abs.False, Abs.True,  Abs.False) -> bottom
      (Abs.False, Abs.False, Abs.True)  -> bottom
      (Abs.Top,   Abs.True,  Abs.False) -> bottom
      (_, _, _)                         -> return (i, j)

    eqᴮ i j x = case x of
      Abs.True
        | i ⊑ j                               -> return (i, i)
        | j ⊑ i                               -> return (j, j)
        | i == j                              -> return (i, j)
        | otherwise                           -> bottom
      Abs.False -> case (i, j) of
        (Sign AbsS.Top,     Sign _)           -> return (i, j)
        (Sign _,            Sign AbsS.Top)    -> return (i, j)
        (BoolVal Abs.Top,   BoolVal Abs.True) -> return (BoolVal Abs.False, BoolVal Abs.True)
        (BoolVal Abs.Top,   BoolVal Abs.False)-> return (BoolVal Abs.True, BoolVal Abs.False)
        (BoolVal Abs.True,  BoolVal Abs.Top)  -> return (BoolVal Abs.True, BoolVal Abs.False)
        (BoolVal Abs.False, BoolVal Abs.Top)  -> return (BoolVal Abs.False, BoolVal Abs.True)
        _ -> if i == j then                      bottom
             else                                return (i, j)
      Abs.Top                                 -> return (i, j)


    notᴮ i x = case x of
      Abs.True -> case i of
        Abs.Top   -> return Abs.False
        Abs.False -> return Abs.False
        _         -> bottom
      Abs.False -> case i of
        Abs.Top   -> return Abs.True
        Abs.True  -> return Abs.True
        _         -> bottom
      _           -> return i

    leqᴮ i j x = case (i, j, x) of
      (AbsS.Negative,  AbsS.Top,       Abs.False) -> return (AbsS.Negative, AbsS.Negative)
      (AbsS.Zero,      AbsS.Top,       Abs.False) -> return (AbsS.Zero,     AbsS.Negative)
      (AbsS.Positive,  AbsS.Top,       Abs.True)  -> return (AbsS.Positive, AbsS.Positive)
      (AbsS.Top,       AbsS.Negative,  Abs.True)  -> return (AbsS.Negative, AbsS.Negative)
      (AbsS.Top,       AbsS.Positive,  Abs.False) -> return (AbsS.Positive, AbsS.Positive)
      (AbsS.Negative,  AbsS.Zero,      Abs.False) -> bottom
      (AbsS.Negative,  AbsS.Positive,  Abs.False) -> bottom
      (AbsS.Zero,      AbsS.Negative,  Abs.True)  -> bottom
      (AbsS.Zero,      AbsS.Zero,      Abs.False) -> bottom
      (AbsS.Zero,      AbsS.Positive,  Abs.False) -> bottom
      (AbsS.Positive,  AbsS.Negative,  Abs.True)  -> bottom
      (AbsS.Positive,  AbsS.Zero,      Abs.True)  -> bottom
      (_, _, _)                                   -> return (i, j)

    typeOfᴮ i j x = case (i, j, x) of
      (Sign _,    TypeVal AbsT.Number,  BoolVal Abs.False) -> bottom
      (BoolVal _, TypeVal AbsT.Boolean, BoolVal Abs.False) -> bottom
      (BoolVal _, TypeVal AbsT.Number,  BoolVal Abs.True)  -> bottom
      (Sign _,    TypeVal AbsT.Boolean, BoolVal Abs.True)  -> bottom
      (Top,       TypeVal AbsT.Number,  BoolVal Abs.True)  -> bottom
      (Top,       TypeVal AbsT.Boolean, BoolVal Abs.True)  -> return (BoolVal Abs.Top, TypeVal AbsT.Boolean)
      (Sign _,    TypeVal AbsT.Top,     BoolVal Abs.True)  -> return (i, TypeVal AbsT.Number)
      (BoolVal _, TypeVal AbsT.Top,     BoolVal Abs.True)  -> return (i, TypeVal AbsT.Boolean)
      _                                                    -> return (i, j)



class ElementOf x xs where
  (∈) :: x -> xs -> Bool

instance ElementOf Int Val where
  _ ∈ Top                   = True
  n ∈ Sign AbsS.Positive         = n > 0
  n ∈ Sign AbsS.Negative         = n < 0
  n ∈ Sign AbsS.Zero        = n == 0
  _ ∈ _                     = False

instance ElementOf Type Val where
  _        ∈ Top                  = True
  _        ∈ TypeVal AbsT.Top     = True
  Boolean  ∈ TypeVal AbsT.Boolean = True
  Number   ∈ TypeVal AbsT.Boolean = True
  _        ∈ _                    = False

instance ElementOf Bool Val where
  _     ∈ Top   = True
  _     ∈ BoolVal Abs.Top   = True
  True  ∈ BoolVal Abs.True  = True
  False ∈ BoolVal Abs.False = True
  _     ∈ _                 = False

instance PreOrd Val where
  _         ⊑ Top       = True
  BoolVal x ⊑ BoolVal y = x ⊑ y
  Sign x    ⊑ Sign y    = x ⊑ y
  _         ⊑ _         = False

instance UpperBounded Val where
  top = Top

instance Complete Val where
  Top       ⊔ _         = Top
  _         ⊔ Top       = Top
  BoolVal x ⊔ BoolVal y = BoolVal (x ⊔ y)
  Sign x    ⊔ Sign y    = Sign (x ⊔ y)
  _ ⊔ _                 = Top

instance CoComplete (FreeCocompletion Env) where
  Bottom   ⊓ _        = Bottom
  _        ⊓ Bottom   = Bottom
  Upper x0 ⊓ Upper y0 = Upper (M.unionWith (⊓) x0 y0)

instance CoComplete (FreeCocompletion Val) where
  Bottom   ⊓ _        = Bottom
  _        ⊓ Bottom   = Bottom
  Upper x0 ⊓ Upper y0 = case (x0,y0) of
    (x,                  Top)                -> Upper x
    (Top,                y)                  -> Upper y
    (BoolVal Abs.Top,    BoolVal y)          -> Upper $ BoolVal y
    (BoolVal x,          BoolVal Abs.Top)    -> Upper $ BoolVal x
    (BoolVal Abs.True,   BoolVal Abs.True)   -> Upper $ BoolVal Abs.True
    (BoolVal Abs.False,  BoolVal Abs.False)  -> Upper $ BoolVal Abs.False
    (Sign AbsS.Top,      Sign x)             -> Upper $ Sign x
    (Sign x,             Sign AbsS.Top)      -> Upper $ Sign x
    (Sign AbsS.Positive, Sign AbsS.Positive) -> Upper $ Sign AbsS.Positive
    (Sign AbsS.Negative, Sign AbsS.Negative) -> Upper $ Sign AbsS.Negative
    (Sign AbsS.Zero,     Sign AbsS.Zero)     -> Upper $ Sign AbsS.Zero
    (TypeVal AbsT.Top,   TypeVal y)          -> Upper $ TypeVal y
    (TypeVal x, TypeVal AbsT.Top)            -> Upper $ TypeVal x
    (TypeVal x, TypeVal y) | x == y          -> Upper $ TypeVal x
    (_, _)                                   -> Bottom

instance Show Val where
  show (BoolVal x)                     = show x
  show Top                             = "Top"
  show (Sign x)                        = show x
  show (TypeVal x)                     = show x

instance Pretty Val where
  pretty = viaShow

type IV = Interval Int

sNegative :: Val
sNegative = Sign AbsS.Negative

sZero :: Val
sZero = Sign AbsS.Zero

sPositive :: Val
sPositive = Sign AbsS.Positive

sTop :: Val
sTop = Sign AbsS.Top

bTrue :: Val
bTrue = BoolVal Abs.True

bFalse :: Val
bFalse = BoolVal Abs.False

bTop :: Val
bTop = BoolVal Abs.Top

tBoolean :: Val
tBoolean = TypeVal AbsT.Boolean

tNumber :: Val
tNumber = TypeVal AbsT.Number

tTop :: Val
tTop = TypeVal AbsT.Top
