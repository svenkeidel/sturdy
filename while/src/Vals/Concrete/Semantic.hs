{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Vals.Concrete.Semantic where

import Prelude (String, Double, Maybe(..), Bool(..), Eq(..), Num(..), (&&), (||), (/), ($), (.), fst)
import qualified Prelude as Prelude

import WhileLanguage (HasStore(..), HasRandomGen(..), Statement, Expr, Label)
import qualified WhileLanguage as L
import Vals.Concrete.Val

import Data.Error
import qualified Data.Map as Map
import Data.Text (Text)

import Control.Arrow
import Control.Arrow.Fail
import Control.Arrow.Utils

import Control.Monad.State hiding (State)
import Control.Monad.Except

import System.Random

-----------
-- Eval
-----------

lookup :: (ArrowChoice c, ArrowFail String c, HasStore c Store) => c (Text,Label) Val
lookup = proc (x,l) -> do
  store <- getStore -< x
  case Map.lookup x store of
    Just v -> returnA -< v
    Nothing -> failA -< "variable not found"

boolLit :: Arrow c => c (Bool,Label) Val
boolLit = arr (\(b,l) -> BoolVal b)

and :: (ArrowChoice c, ArrowFail String c) => c (Val,Val,Label) Val
and = proc (v1,v2,l) -> case (v1,v2) of
  (BoolVal b1,BoolVal b2) -> returnA -< BoolVal (b1 && b2)
  _ -> failA -< "Expected two booleans as arguments for 'and'"

or :: (ArrowChoice c, ArrowFail String c) => c (Val,Val,Label) Val
or = proc (v1,v2,l) -> case (v1,v2) of
  (BoolVal b1,BoolVal b2) -> returnA -< BoolVal (b1 || b2)
  _ -> failA -< "Expected two booleans as arguments for 'or'"

not :: (ArrowChoice c, ArrowFail String c) => c (Val,Label) Val
not = proc (v,l) -> case v of
  BoolVal b -> returnA -< BoolVal (Prelude.not b)
  _ -> failA -< "Expected a boolean as argument for 'not'"

numLit :: Arrow c => c (Double,Label) Val
numLit = arr (\(d,l) -> NumVal d)

randomNum :: (Arrow c, HasRandomGen c) => c Label Val
randomNum = proc l -> do
  n <- nextRandom -< ()
  returnA -< NumVal n

add :: (ArrowChoice c, ArrowFail String c) => c (Val,Val,Label) Val
add = proc (v1,v2,l) -> case (v1,v2) of
  (NumVal n1,NumVal n2) -> returnA -< NumVal (n1 + n2)
  _ -> failA -< "Expected two numbers as arguments for 'add'"

sub :: (ArrowChoice c, ArrowFail String c) => c (Val,Val,Label) Val
sub = proc (v1,v2,l) -> case (v1,v2) of
  (NumVal n1,NumVal n2) -> returnA -< NumVal (n1 - n2)
  _ -> failA -< "Expected two numbers as arguments for 'sub'"

mul :: (ArrowChoice c, ArrowFail String c) => c (Val,Val,Label) Val
mul = proc (v1,v2,l) -> case (v1,v2) of
  (NumVal n1,NumVal n2) -> returnA -< NumVal (n1 * n2)
  _ -> failA -< "Expected two numbers as arguments for 'mul'"

div :: (ArrowChoice c, ArrowFail String c) => c (Val,Val,Label) Val
div = proc (v1,v2,l) -> case (v1,v2) of
  (NumVal n1,NumVal n2) -> returnA -< NumVal (n1 / n2)
  _ -> failA -< "Expected two numbers as arguments for 'mul'"

eq :: (ArrowChoice c, ArrowFail String c) => c (Val,Val,Label) Val
eq = proc (v1,v2,l) -> case (v1,v2) of
  (NumVal n1,NumVal n2)   -> returnA -< BoolVal (n1 == n2)
  (BoolVal b1,BoolVal b2) -> returnA -< BoolVal (b1 == b2)
  _ -> failA -< "Expected two values of the same type as arguments for 'eq'"

fixEval :: Arrow c => (c Expr v -> c Expr v) -> c Expr v
fixEval f = f (fixEval f)



----------
-- Run
----------

fixRun :: ArrowChoice c => (c [Statement] () -> c Statement ()) -> c [Statement] ()
fixRun f = voidA $ mapA $ f (fixRun f)

store :: (ArrowChoice c, HasStore c Store) => c (Text,Val,Label) ()
store = proc (x,v,_) -> modifyStore -< (Map.insert x v)

if_ :: (ArrowChoice c, ArrowFail String c) => c [Statement] () -> c [Statement] () -> c (Val,([Statement],[Statement]),Label) ()
if_ f1 f2 = proc (v,(x,y),_) -> case v of
  BoolVal True -> f1 -< x
  BoolVal False -> f2 -< y
  _ -> failA -< "Expected boolean as argument for 'if'"



----------
-- Arrows
----------

type State = (Store,StdGen)
type M = StateT State (Except String)
runM :: [Statement] -> Error String ((),State)
runM ss = fromEither $ runExcept $ runStateT (runKleisli L.run ss) (initStore, mkStdGen 0)

run :: [Statement] -> Error String (Store,())
run = fmap (\(_,(st,rnd)) -> (st,())) . runM

runLifted :: [Statement] -> Error String (LiftedStore,())
runLifted = fmap (\(st,pr) -> (liftStore st,pr)) . run

instance L.HasStore (Kleisli M) Store where
  getStore = Kleisli $ \_ -> get >>= return . fst
  putStore = Kleisli $ \st -> modify (\(_,rnd) -> (st,rnd))
  modifyStore = Kleisli  $ \f -> modify (\(st,rnd) -> (f st,rnd))

instance L.HasRandomGen (Kleisli M) where
  nextRandom = Kleisli $ \() -> do
    (st, gen) <- get
    let (r, gen') = random gen
    put (st, gen')
    return r

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
