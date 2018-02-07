{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Props.ControlFlow.Interval where

import WhileLanguage (HasStore(..), HasProp(..), Statement, Expr, Label)
import qualified WhileLanguage as L

import Vals.Interval.Val
import qualified Vals.Interval.Semantics as Interval

import Props.ControlFlow.Prop

import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Error
import Data.Order

import Control.Arrow
import Control.Arrow.Fail
import Control.Arrow.Utils

import Control.Monad.State hiding (State)
import Control.Monad.Except

import System.Random

store :: (ArrowChoice c, HasStore c Store, HasProp c (AProp Val)) => c (Text,Val,Label) ()
store = (proc (x,v,l) -> modifyProp -< pushNode $ CFGAssign l v)
    &&> Interval.store

if_ :: (ArrowChoice c, ArrowFail String c, Complete (c ([L.Statement],[L.Statement]) ()), HasProp c (AProp Val))
    => c [L.Statement] () -> c [L.Statement] () -> c (Val,([L.Statement],[L.Statement]),L.Label) ()
if_ f1 f2 = (proc (v,(xs,ys),l) -> modifyProp -< pushNode $ CFGIf l v)
        &&> Interval.if_ f1 f2

----------
-- Arrows
----------

type State = (Store,AProp Val)
type M = StateT State (Except String)
runM :: [Statement] -> Error String ((),State)
runM ss = fromEither $ runExcept $ runStateT (runKleisli L.run ss) (initStore,initAProp)

run :: [Statement] -> Error String (Store,AProp Val)
run = fmap (\(_,(st,pr)) -> (st,pr)) . runM

instance L.HasStore (Kleisli M) Store where
  getStore = Kleisli $ \_ -> get >>= return . fst
  putStore = Kleisli $ \st -> modify (\(_,y) -> (st,y))
  modifyStore = Kleisli $ \f -> modify (\(st,y) -> (f st,y))

instance L.HasProp (Kleisli M) (AProp Val) where
  getProp = Kleisli $ \_ -> get >>= return . snd
  putProp = Kleisli $ \pr -> modify (\(x,_) -> (x,pr))
  modifyProp = Kleisli $ \f -> modify (\(x,pr) -> (x,f pr))

instance L.Eval (Kleisli M) Val  where
  lookup = Interval.lookup
  boolLit = Interval.boolLit
  and = Interval.and
  or = Interval.or
  not = Interval.not
  numLit = Interval.numLit
  randomNum = Interval.randomNum
  add = Interval.add
  sub = Interval.sub
  mul = Interval.mul
  div = Interval.div
  eq = Interval.eq
  fixEval = Interval.fixEval

instance L.Run (Kleisli M) Val where
  fixRun = Interval.fixRun
  store = store
  if_ = if_
