{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Props.FailedReads.Interval where

import Prelude (String, Maybe(..), ($), (.), fst, snd, fmap, uncurry)

import WhileLanguage (HasStore(..), HasProp(..), Statement, Label)
import qualified WhileLanguage as L

import Vals.Interval.Val
import qualified Vals.Interval.Semantics as Interval

import Props.FailedReads.Prop

import Data.Error
import Data.Text (Text)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Arrow
import Control.Arrow.Fail
import Control.Arrow.Fix
import Control.Arrow.State


lookup :: (ArrowChoice c, ArrowFail String c, HasStore c Store, HasProp c AProp) => c (Text,Label) Val
lookup = proc (x,_) -> do
  store <- getStore -< ()
  case Map.lookup x store of
    Just v -> returnA -< v
    Nothing -> do
      modifyProp (arr $ uncurry Set.insert) -< x
      failA -< "variable not found"


----------
-- Arrows
----------

type State = (Store,AProp)
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

instance L.HasProp M AProp where
  getProp = getA >>> arr snd
  putProp = modifyA $ arr $ \(pr,(st,_)) -> (st,pr)

instance L.Eval M Val  where
  lookup = lookup
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
  store = Interval.store
  if_ = Interval.if_