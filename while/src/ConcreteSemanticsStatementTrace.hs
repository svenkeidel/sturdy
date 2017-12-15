{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Arrows #-}
module ConcreteSemanticsStatementTrace where

import WhileLanguage

import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)

import Data.Set (Set)
import qualified Data.Set as Set

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer
import Control.Arrow
import Control.Arrow.Fail

data Val = BoolVal Bool | NumVal Double
type Store = Map Text Val

data TraceElem = TrStore Text | TrIfThen | TrIfElse | TrEndIf
type Prop = [TraceElem]
type M = StateT (Store,Prop) (Except String)

runConcrete :: Kleisli M [Statement] ()
runConcrete = run

propConcrete :: Kleisli M [Statement] Prop
propConcrete = proc ss -> do
  (_,prop) <- getA <<< run -< ss
  returnA -< reverse prop

getA :: Kleisli M () (Store,Prop)
getA = Kleisli (\_ -> get)

getStore :: M Store
getStore = get >>= return . fst

putStore :: Store -> M ()
putStore env = modify (\(x,y) -> (env,y))

modifyStore :: (Store -> Store) -> M ()
modifyStore f = modify (\(x,y) -> (f x, y))

putProp :: Prop -> M ()
putProp prop = modify (\(x,y) -> (x,prop))

modifyProp :: (Prop -> Prop) -> M ()
modifyProp f = modify (\(x,y) -> (x, f y))

instance ArrowFail String (Kleisli M) where
  failA = Kleisli $ \e -> throwError e
          
instance Run (Kleisli M) Val where
  fixRun f = voidA $ mapA $ f (fixRun f)

  store = Kleisli $ \(x,v) -> do
    modifyProp (TrStore x :)
    modifyStore (M.insert x v)

  if_ (Kleisli f1) (Kleisli f2) = Kleisli (\(v,(x,y)) -> case v of
    BoolVal True -> do
      modifyProp (TrIfThen :)
      f1 x
      modifyProp (TrEndIf :)
    BoolVal False -> do
      modifyProp (TrIfElse :)
      f2 y
      modifyProp (TrEndIf :)
    _ -> throwError "Expected boolean as argument for 'if'")

instance Eval (Kleisli M) Val where
  lookup = Kleisli $ \x -> do
    env <- getStore
    case M.lookup x env of
      Just v -> return v
      Nothing -> throwError "variable not found"

  boolLit = arr BoolVal

  and = proc (v1,v2) -> case (v1,v2) of
    (BoolVal b1,BoolVal b2) -> returnA -< BoolVal (b1 && b2)
    _ -> failA -< "Expected two booleans as arguments for 'and'"

  or = proc (v1,v2) -> case (v1,v2) of
    (BoolVal b1,BoolVal b2) -> returnA -< BoolVal (b1 || b2)
    _ -> failA -< "Expected two booleans as arguments for 'or'"

  not = proc v -> case v of
    BoolVal b -> returnA -< BoolVal (Prelude.not b)
    _ -> failA -< "Expected a boolean as argument for 'not'"

  numLit = arr NumVal

  add = proc (v1,v2) -> case (v1,v2) of
    (NumVal n1,NumVal n2) -> returnA -< NumVal (n1 + n2)
    _ -> failA -< "Expected two numbers as arguments for 'add'"

  sub = proc (v1,v2) -> case (v1,v2) of
    (NumVal n1,NumVal n2) -> returnA -< NumVal (n1 - n2)
    _ -> failA -< "Expected two numbers as arguments for 'sub'"

  mul = proc (v1,v2) -> case (v1,v2) of
    (NumVal n1,NumVal n2) -> returnA -< NumVal (n1 * n2)
    _ -> failA -< "Expected two numbers as arguments for 'mul'"

  div = proc (v1,v2) -> case (v1,v2) of
    (NumVal n1,NumVal n2) -> returnA -< NumVal (n1 / n2)
    _ -> failA -< "Expected two numbers as arguments for 'mul'"

  eq = proc (v1,v2) -> case (v1,v2) of
    (NumVal n1,NumVal n2)   -> returnA -< BoolVal (n1 == n2)
    (BoolVal b1,BoolVal b2) -> returnA -< BoolVal (b1 == b2)
    _ -> failA -< "Expected two values of the same type as arguments for 'eq'"

  fixEval f = f (fixEval f)


