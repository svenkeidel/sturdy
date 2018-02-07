{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Vals.Interval.Semantics where

import Prelude (String, Double, Maybe(..), Bool(..), Eq(..), Num(..), (&&), (/), const, ($), (.))

import WhileLanguage (HasStore(..), Statement, Label)
import qualified WhileLanguage as L
import Vals.Interval.Val
import qualified Data.Interval as I

import Data.Order
import Data.Error
import qualified Data.Map as Map
import Data.Text (Text)

import Control.Arrow
import Control.Arrow.Fail
import Control.Arrow.Utils

import Control.Monad.State hiding (State)
import Control.Monad.Except

-----------
-- Eval
-----------

lookup :: (ArrowChoice c, ArrowFail String c, HasStore c Store) => c (Text,Label) Val
lookup = proc (x,_) -> do
  store <- getStore -< x
  case Map.lookup x store of
    Just v -> returnA -< v
    Nothing -> failA -< "variable not found"

boolLit :: Arrow c => c (Bool,Label) Val
boolLit = arr $ \(b,_) -> BoolVal b

and :: (ArrowChoice c, ArrowFail String c) => c (Val,Val,Label) Val
and = proc (v1,v2,_) -> case (v1,v2) of
  (BoolVal False,_) -> returnA -< BoolVal False
  (_,BoolVal False) -> returnA -< BoolVal False
  (BoolVal True,BoolVal True) -> returnA -< BoolVal True
  (Top,_) -> returnA -< Top
  (_,Top) -> returnA -< Top
  _ -> failA -< "Expected two booleans as arguments for 'and'"

or :: (ArrowChoice c, ArrowFail String c) => c (Val,Val,Label) Val
or = proc (v1,v2,_) -> case (v1,v2) of
  (BoolVal True,_) -> returnA -< BoolVal True
  (_,BoolVal True) -> returnA -< BoolVal True
  (BoolVal False,BoolVal False) -> returnA -< BoolVal False
  (Top,_) -> returnA -< Top
  (_,Top) -> returnA -< Top
  _ -> failA -< "Expected two booleans as arguments for 'or'"

not :: (ArrowChoice c, ArrowFail String c) => c (Val,Label) Val
not = proc (v,_) -> case v of
  BoolVal True -> returnA -< BoolVal False
  BoolVal False -> returnA -< BoolVal True
  Top -> returnA -< Top
  _ -> failA -< "Expected a boolean as argument for 'not'"

numLit :: Arrow c => c (Double,Label) Val
numLit = arr $ \(x,_) -> NumVal (I.Interval x x)

randomNum :: Arrow c => c Label Val
randomNum = arr $ const $ NumVal top

add :: (ArrowChoice c, ArrowFail String c) => c (Val,Val,Label) Val
add = proc (v1,v2,_) -> case (v1,v2) of
  (NumVal n1,NumVal n2) -> returnA -< NumVal (n1 + n2)
  (Top,_) -> returnA -< Top
  (_,Top) -> returnA -< Top
  _ -> failA -< "Expected two numbers as arguments for 'add'"

sub :: (ArrowChoice c, ArrowFail String c) => c (Val,Val,Label) Val
sub = proc (v1,v2,_) -> case (v1,v2) of
  (NumVal n1,NumVal n2) -> returnA -< NumVal (n1 - n2)
  (Top,_) -> returnA -< Top
  (_,Top) -> returnA -< Top
  _ -> failA -< "Expected two numbers as arguments for 'sub'"

mul:: (ArrowChoice c, ArrowFail String c) => c (Val,Val,Label) Val
mul = proc (v1,v2,_) -> case (v1,v2) of
  (NumVal n1,NumVal n2) -> returnA -< NumVal (n1 * n2)
  (Top,_) -> returnA -< Top
  (_,Top) -> returnA -< Top
  _ -> failA -< "Expected two numbers as arguments for 'mul'"

div :: (ArrowChoice c, ArrowFail String c) => c (Val,Val,Label) Val
div = proc (v1,v2,_) -> case (v1,v2) of
  (NumVal n1,NumVal n2) -> returnA -< NumVal (n1 / n2)
  (Top,_) -> returnA -< Top
  (_,Top) -> returnA -< Top
  _ -> failA -< "Expected two numbers as arguments for 'mul'"

eq :: (ArrowChoice c, ArrowFail String c) => c (Val,Val,Label) Val
eq = proc (v1,v2,_) -> case (v1,v2) of
  (NumVal (I.Interval m1 m2),NumVal (I.Interval n1 n2)) | m1 == m2 && n1 == n2 -> returnA -< BoolVal (m1 == n1)
  (NumVal _,NumVal _)   -> returnA -< Top
  (BoolVal b1,BoolVal b2) -> returnA -< BoolVal (b1 == b2)
  (Top,_) -> returnA -< Top
  (_,Top) -> returnA -< Top
  _ -> failA -< "Expected two values of the same type as arguments for 'eq'"

fixEval :: Arrow c => (c L.Expr v -> c L.Expr v) -> c L.Expr v
fixEval f = f (fixEval f)



----------
-- Run
----------

fixRun :: ArrowChoice c => (c [L.Statement] () -> c L.Statement ()) -> c [L.Statement] ()
fixRun f = voidA $ mapA $ f (fixRun f)

store :: (ArrowChoice c, HasStore c Store) => c (Text,Val,L.Label) ()
store = proc (x,v,_) -> modifyStore -< (Map.insert x v)

if_ :: (ArrowChoice c, ArrowFail String c, Complete (c ([L.Statement],[L.Statement]) ()))
    => c [L.Statement] () -> c [L.Statement] () -> c (Val,([L.Statement],[L.Statement]),L.Label) ()
if_ f1 f2 = proc (v,(x,y),_) -> case v of
  BoolVal True -> f1 -< x
  BoolVal False -> f2 -< y
  Top -> joined f1 f2 -< (x,y)
  _ -> failA -< "Expected boolean as argument for 'if'"

----------
-- Arrows
----------

type State = Store
type M = StateT State (Except String)
runM :: [Statement] -> Error String ((),State)
runM ss = fromEither $ runExcept $ runStateT (runKleisli L.run ss) initStore

run :: [Statement] -> Error String (Store,())
run = fmap (\(_,st) -> (st,())) . runM

instance HasStore (Kleisli M) Store where
  getStore = Kleisli $ \_ -> get
  putStore = Kleisli $ \st -> modify $ const st
  modifyStore = Kleisli  $ \f -> modify f

instance L.Eval (Kleisli M) Val  where
  lookup = lookup
  boolLit = boolLit
  and = and
  or = or
  not = not
  numLit = numLit
  randomNum = randomNum
  add = add
  sub = sub
  mul = mul
  div = div
  eq = eq
  fixEval = fixEval

instance L.Run (Kleisli M) Val where
  fixRun = fixRun
  store = store
  if_ = if_
