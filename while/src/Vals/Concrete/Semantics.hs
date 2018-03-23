{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Vals.Concrete.Semantics where

import Prelude (String, Double, Maybe(..), Bool(..), Eq(..), Num(..), (&&), (||), (/), ($), (.), fst,fmap)
import qualified Prelude

import WhileLanguage (HasRandomGen(..), Statement, Expr)
import qualified WhileLanguage as L
import Vals.Concrete.Val

import Data.Label
import Data.Error
import qualified Data.Map as Map
import Data.Text (Text)

import Control.Arrow
import Control.Arrow.Fail
import Control.Arrow.Fix
import Control.Arrow.State

import System.Random

store :: (ArrowChoice c, HasStore c Store) => c (Text,Val,Label) ()
store = modifyStore (arr $ \((x,v,_),st) -> Map.insert x v st)

if_ :: (ArrowChoice c, ArrowFail String c) => c [Statement] () -> c [Statement] () -> c (Val,([Statement],[Statement]),Label) ()
if_ f1 f2 = proc (v,(x,y),_) -> case v of
  BoolVal True -> f1 -< x
  BoolVal False -> f2 -< y
  _ -> failA -< "Expected boolean as argument for 'if'"


type State = (Store,StdGen)
initState :: State
initState = (initStore, mkStdGen 0)

type Interp = StateArrow State (ErrorArrow String Fix)

runInterp :: [Statement] -> Error String (State,())
runInterp ss = runFix (runErrorArrow (runStateArrow L.run)) (initState, ss)

run :: [Statement] -> Error String (Store,())
run = fmap (first fst) . runInterp

runLifted :: [Statement] -> Error String (LiftedStore,())
runLifted = fmap (first liftStore) . run

instance L.HasRandomGen Interp where
  nextRandom = proc () -> do
    (st, gen) <- getA -< ()
    let (r, gen') = random gen
    putA -< (st, gen')
    returnA -< r

instance L.Eval Interp Val  where
  lookup = proc (x,l) -> do
    st <- getStore -< ()
    case Map.lookup x st of
      Just v -> returnA -< v
      Nothing -> failA -< "variable not found"
  boolLit = arr (\(b,l) -> BoolVal b)
  and = proc (v1,v2,l) -> case (v1,v2) of
    (BoolVal b1,BoolVal b2) -> returnA -< BoolVal (b1 && b2)
    _ -> failA -< "Expected two booleans as arguments for 'and'"
  or = proc (v1,v2,l) -> case (v1,v2) of
    (BoolVal b1,BoolVal b2) -> returnA -< BoolVal (b1 || b2)
    _ -> failA -< "Expected two booleans as arguments for 'or'"
  not = proc (v,l) -> case v of
    BoolVal b -> returnA -< BoolVal (Prelude.not b)
    _ -> failA -< "Expected a boolean as argument for 'not'"
  numLit = arr (\(d,l) -> NumVal d)
  randomNum = proc l -> do
    n <- nextRandom -< ()
    returnA -< NumVal n
  add = proc (v1,v2,l) -> case (v1,v2) of
    (NumVal n1,NumVal n2) -> returnA -< NumVal (n1 + n2)
    _ -> failA -< "Expected two numbers as arguments for 'add'"
  sub = proc (v1,v2,l) -> case (v1,v2) of
    (NumVal n1,NumVal n2) -> returnA -< NumVal (n1 - n2)
    _ -> failA -< "Expected two numbers as arguments for 'sub'"
  mul = proc (v1,v2,l) -> case (v1,v2) of
    (NumVal n1,NumVal n2) -> returnA -< NumVal (n1 * n2)
    _ -> failA -< "Expected two numbers as arguments for 'mul'"
  div = proc (v1,v2,l) -> case (v1,v2) of
    (NumVal n1,NumVal n2) -> returnA -< NumVal (n1 / n2)
    _ -> failA -< "Expected two numbers as arguments for 'mul'"
  eq = proc (v1,v2,l) -> case (v1,v2) of
    (NumVal n1,NumVal n2)   -> returnA -< BoolVal (n1 == n2)
    (BoolVal b1,BoolVal b2) -> returnA -< BoolVal (b1 == b2)
    _ -> failA -< "Expected two values of the same type as arguments for 'eq'"


instance L.Run Interp Val where
  store = store
  if_ = if_

instance ArrowFix Expr Val Interp where
  fixA f = f (fixA f)
