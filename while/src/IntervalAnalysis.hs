{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Arrows #-}
module IntervalAnalysis where

import WhileLanguage
import qualified ConcreteSemantics as Concrete

import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import Data.Interval
import Data.Error
import Data.Order
import Data.GaloisConnection
import Data.Powerset
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import Control.Monad.State
import Control.Monad.Except
import Control.Arrow
import Control.Arrow.Fail
import Control.Arrow.Utils

import Vals.Interval.Val

type M = StateT Store (Except String)
runM :: [Statement] -> Error String ((), Store)
runM ss = fromEither $ runExcept $ runStateT (runKleisli run ss) initStore

runConcrete :: [Statement] -> Error String Store
runConcrete ss = fmap snd $ runM ss

runAbstract :: Kleisli M [Statement] ()
runAbstract = run

getStore :: M Store
getStore = get

putStore :: Store -> M ()
putStore env = modify (const env)

modifyStore :: (Store -> Store) -> M ()
modifyStore f = modify f

instance Run (Kleisli M) Val where
  fixRun f = voidA $ mapA $ f (fixRun f)

  store = Kleisli $ \(x,v,_) -> modifyStore (M.insert x v)

  if_ f1 f2 = proc (v,(x,y),_) -> case v of
    BoolVal True -> f1 -< x
    BoolVal False -> f2 -< y
    Top -> (f1 -< x) ⊔ (f2 -< y)
    _ -> failA -< "Expected boolean as argument for 'if'"

instance Eval (Kleisli M) Val where
  lookup = Kleisli $ \x -> do
    env <- getStore
    case M.lookup x env of
      Just v -> return v
      Nothing -> throwError "variable not found"

  boolLit = arr BoolVal

  and = proc (v1,v2) -> case (v1,v2) of
    (BoolVal False,_) -> returnA -< BoolVal False
    (_,BoolVal False) -> returnA -< BoolVal False
    (BoolVal True,BoolVal True) -> returnA -< BoolVal True
    (Top,_) -> returnA -< Top
    (_,Top) -> returnA -< Top
    _ -> failA -< "Expected two booleans as arguments for 'and'"

  or = proc (v1,v2) -> case (v1,v2) of
    (BoolVal True,_) -> returnA -< BoolVal True
    (_,BoolVal True) -> returnA -< BoolVal True
    (BoolVal False,BoolVal False) -> returnA -< BoolVal False
    (Top,_) -> returnA -< Top
    (_,Top) -> returnA -< Top
    _ -> failA -< "Expected two booleans as arguments for 'or'"
  
  not = proc v -> case v of
    BoolVal True -> returnA -< BoolVal False
    BoolVal False -> returnA -< BoolVal True
    Top -> returnA -< Top
    _ -> failA -< "Expected a boolean as argument for 'not'"

  numLit = arr $ \x -> NumVal (fromIntegral x)

  add = proc (v1,v2) -> case (v1,v2) of
    (NumVal n1,NumVal n2) -> returnA -< NumVal (n1 + n2)
    (Top,_) -> returnA -< Top
    (_,Top) -> returnA -< Top
    _ -> failA -< "Expected two numbers as arguments for 'add'"

  sub = proc (v1,v2) -> case (v1,v2) of
    (NumVal n1,NumVal n2) -> returnA -< NumVal (n1 - n2)
    (Top,_) -> returnA -< Top
    (_,Top) -> returnA -< Top
    _ -> failA -< "Expected two numbers as arguments for 'sub'"

  mul = proc (v1,v2) -> case (v1,v2) of
    (NumVal n1,NumVal n2) -> returnA -< NumVal (n1 * n2)
    (Top,_) -> returnA -< Top
    (_,Top) -> returnA -< Top
    _ -> failA -< "Expected two numbers as arguments for 'mul'"

  div = proc (v1,v2) -> case (v1,v2) of
    (NumVal n1,NumVal n2) -> returnA -< NumVal (n1 / n2)
    (Top,_) -> returnA -< Top
    (_,Top) -> returnA -< Top
    _ -> failA -< "Expected two numbers as arguments for 'mul'"

  eq = proc (v1,v2) -> case (v1,v2) of
    (NumVal n1,NumVal n2)   -> returnA -< BoolVal (n1 == n2)
    (BoolVal b1,BoolVal b2) -> returnA -< BoolVal (b1 == b2)
    (Top,_) -> returnA -< Top
    (_,Top) -> returnA -< Top
    _ -> failA -< "Expected two values of the same type as arguments for 'eq'"

  fixEval f = f (fixEval f)
