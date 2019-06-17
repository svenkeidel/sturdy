{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
-- | This is an abstract interpreter in monadic style. It calculates
-- the range of numbers an program can evaluate too, without running
-- the program for each number in that program.
module MonadicStyle.AbstractInterpreter where

import           Prelude hiding (Bool(..),lookup)
import qualified Prelude as P

import           Syntax

import           Control.Monad

import           Data.Order
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Label
import           Data.Maybe
import           Data.Abstract.DiscretePowerset(Pow)

import           GHC.Exts

data AbsVal   = BoolVal AbsBool | NumVal Interval | TopVal deriving (Eq)
data AbsBool  = True | False | TopBool deriving (Eq)
data Interval = Interval Int Int deriving (Eq)
type Addr     = Label
type Env      = Map String Addr
type Store    = Map Addr AbsVal

newtype M a = M {runM :: Env -> Store -> Either (Pow String) (Store,a)}

-- eval :: Store -> Expr -> Either (Pow String) Val
eval :: Expr -> M AbsVal
eval e = case e of
  Var x _ -> do
    env <- ask
    store <- get
    addr <- lookup x env
    lookup addr store
  NumLit n _ -> return (NumVal (Interval n n))
  Add e1 e2 _ -> do
    v1 <- eval e1
    v2 <- eval e2
    case (v1,v2) of
      (NumVal (Interval i1 j1), NumVal (Interval i2 j2)) ->
        return (NumVal (Interval (i1 + i2) (j1 + j2)))
      (_,_) -> throw "Expected two numbers as arguments for +"
  BoolLit b _ -> case b of
    P.True -> return (BoolVal True)
    P.False -> return (BoolVal False)
  And e1 e2 _ -> do
    v1 <- eval e1
    v2 <- eval e2
    case (v1,v2) of
      (BoolVal b1, BoolVal b2) -> return $ BoolVal $ case (b1,b2) of
        (False,_) -> False
        (_,False) -> False
        (True,_) -> b2
        (_,True) -> b1
        (_,_) -> TopBool
      (TopVal, _) -> return TopVal
      (_,TopVal) -> return TopVal
      (_,_) -> throw "Expected two booleans as arguments for &&"
  Lt e1 e2 _ -> do
    v1 <- eval e1
    v2 <- eval e2
    case (v1,v2) of
      (NumVal (Interval i1 j1), NumVal (Interval i2 j2))
        | j1 < i2   -> return (BoolVal True) 
        | j2 < i1   -> return (BoolVal False) 
        | otherwise -> return (BoolVal TopBool) 
      (TopVal, _) -> return TopVal
      (_,TopVal) -> return TopVal
      (_,_) -> throw "Expected two numbers as arguments for +"

-- run :: Store -> [Statement] -> Either String Store
run :: [Statement] -> M ()
run stmts = case stmts of
  (Assign x e l : rest) -> do
    v <- eval e
    env <- ask
    st <- get
    let addr = fromMaybe l (M.lookup x env)
    put (M.insert addr v st)
    local (run rest) (M.insert x addr env)
  (If cond ifBranch elseBranch _ : rest) -> do
    v <- eval cond
    case v of
      BoolVal True -> run ifBranch
      BoolVal False -> run elseBranch
      BoolVal TopBool -> run ifBranch ⊔ run elseBranch
      NumVal _ -> throw "Expected a boolean expression as condition for an if"
      TopVal -> run ifBranch ⊔ run elseBranch ⊔ throw "Expected a boolean expression as condition for an if"
    run rest
  (While cond body l : rest) ->
    run (If cond (body ++ [While cond body l]) [] l : rest)
  [] ->
    return ()

-- Monadic helper functions ---------------------------------------------
lookup :: Ord a => a -> Map a b -> M b
lookup a m = case M.lookup a m of
  Just b -> return b
  Nothing -> throw "Variable not in scope"

ask :: M Env
ask = M (\env st -> Right (st,env))

local :: M a -> Env -> M a
local (M f) env = M (\_ st -> f env st)

get :: M Store
get = M (\_ st -> Right (st,st))

put :: Store -> M ()
put st = M (\_ _ -> Right (st,()))

throw :: String -> M a
throw er = M (\_ _ -> Left (fromString er))

-- Monad Instances ------------------------------------------------------
deriving instance Functor M

instance Monad M where
  return a = M (\_ st -> Right (st,a))
  M m >>= k = M $ \env st ->
    case m env st of
      Left er -> Left er
      Right (st',x) -> runM (k x) env st'

instance Applicative M where
  pure = return
  (<*>) = ap

-- Orderings ------------------------------------------------------------
instance PreOrd AbsVal where
  NumVal i1 ⊑ NumVal i2 = i1 ⊑ i2
  BoolVal b1 ⊑ BoolVal b2 = b1 ⊑ b2
  _ ⊑ TopVal = P.True
  _ ⊑ _ = P.False

instance PreOrd Interval where
  Interval x1 y1 ⊑ Interval x2 y2 = x2 <= x1 && y1 <= y2

instance PreOrd AbsBool where
  True ⊑ True = P.True
  False ⊑ False = P.True
  _ ⊑ TopBool=  P.True
  _ ⊑ _ = P.False

instance Complete AbsVal where
  NumVal i1 ⊔ NumVal i2 = NumVal (i1 ⊔ i2)
  BoolVal b1 ⊔ BoolVal b2 = BoolVal (b1 ⊔ b2)
  _ ⊔ _ = TopVal

instance Complete Interval where
  Interval x1 y1 ⊔ Interval x2 y2 = Interval (min x1 y1) (max x2 y2)

instance Complete AbsBool where
  b1 ⊔ b2 = if b1 == b2 then b1 else TopBool

instance PreOrd a => PreOrd (M a) where
  _ ⊑ _ = error "non-computable"

instance Complete a => Complete (M a) where
  (M m1) ⊔ (M m2) = M $ \env s -> case (m1 env s,m2 env s) of
    (Left er1, Left er2) -> (Left (er1 ⊔ er2))
    (Left er1, Right _) -> Left er1
    (Right _, Left er2) -> Left er2
    (Right (s1,a1), Right (s2,a2)) -> Right (s1 ⊔ s2, a1 ⊔ a2)
