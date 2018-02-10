{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Props.ControlFlow.Interval where

import WhileLanguage (HasStore(..), HasProp(..), Statement, Label)
import qualified WhileLanguage as L

import Vals.Interval.Val
import qualified Vals.Interval.Semantics as Interval

import Props.ControlFlow.Prop

import Data.Text (Text)
import Data.Error
import Data.Order

import Control.Arrow
import Control.Arrow.Fail
import Control.Arrow.Fix
import Control.Arrow.State
import Control.Arrow.Utils

store :: (ArrowChoice c, HasStore c Store, HasProp c (AProp Val)) => c (Text,Val,Label) ()
store = modifyProp (arr $ \((_,v,l),g) -> pushNode (CFGAssign l v) g)
    &&> Interval.store

if_ :: (ArrowChoice c, ArrowFail String c, Complete (c ([L.Statement],[L.Statement]) ()), HasProp c (AProp Val))
    => c [L.Statement] () -> c [L.Statement] () -> c (Val,([L.Statement],[L.Statement]),L.Label) ()
if_ f1 f2 = modifyProp (arr $ \((v,_,l),g) -> pushNode (CFGIf l v) g)
        &&> Interval.if_ f1 f2

----------
-- Arrows
----------

type State = (Store,AProp Val)
initState :: State
initState = (initStore, initAProp)

type In a = (State,a)
type Out a = Error String (State,a)
type M = StateArrow State (ErrorArrow String (Fix (In [Statement]) (Out ())))

runM :: [Statement] -> Error String (State,())
runM ss = runFix (runErrorArrow (runStateArrow L.run)) (initState, ss)

run :: [Statement] -> Error String (Store,())
run = fmap (first fst) . runM

instance L.HasStore M Store where
  getStore = getA >>> arr fst
  putStore = modifyA $ arr $ \(st,(_,rnd)) -> (st,rnd)

instance L.HasProp M (AProp Val) where
  getProp = getA >>> arr snd
  putProp = modifyA $ arr $ \(pr,(st,_)) -> (st,pr)

instance L.Eval M Val  where
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

instance L.Run M Val where
  store = store
  if_ = if_
