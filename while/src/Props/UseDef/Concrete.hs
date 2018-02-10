{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Props.UseDef.Concrete where

import Prelude (String, ($), (.), fmap)

import WhileLanguage (HasStore(..), HasProp(..), Statement, Label)
import qualified WhileLanguage as L

import Vals.Concrete.Val
import qualified Vals.Concrete.Semantics as Concrete

import Props.UseDef.Prop

import Data.Error
import Data.Text (Text)

import Control.Arrow
import Control.Arrow.Fail
import Control.Arrow.Fix
import Control.Arrow.State
import Control.Arrow.Utils

import System.Random



lookup :: (ArrowChoice c, ArrowFail String c, HasStore c Store, HasProp c Trace) => c (Text,Label) Val
lookup = modifyProp (arr $ \((x,l),tr) -> TrUse x l : tr)
     &&> Concrete.lookup

store :: (ArrowChoice c, HasStore c Store, HasProp c Trace) => c (Text,Val,Label) ()
store = modifyProp (arr $ \((x,_,l),tr) -> TrDef x l : tr)
    &&> Concrete.store

----------
-- Arrows
----------

type State = (Store,Trace,StdGen)
initState :: State
initState = (initStore, initTrace, mkStdGen 0)

type In a = (State,a)
type Out a = Error String (State,a)
type M = StateArrow State (ErrorArrow String (Fix (In [Statement]) (Out ())))

runM :: [Statement] -> Error String (State,())
runM ss = runFix (runErrorArrow (runStateArrow L.run)) (initState, ss)

run :: [Statement] -> Error String (Store,())
run = fmap (first $ \(st,_,_) -> st) . runM

runLifted :: [Statement] -> Error String (LiftedStore,())
runLifted = fmap (first liftStore) . run

instance L.HasStore M Store where
  getStore = getA >>> arr (\(st, _, _) -> st)
  putStore = modifyA $ arr $ \(st,(_,pr,rnd)) -> (st,pr,rnd)

instance L.HasProp M Trace where
  getProp = getA >>> arr (\(_, pr, _) -> pr)
  putProp = modifyA $ arr $ \(pr,(st,_,rnd)) -> (st,pr,rnd)

instance L.HasRandomGen M where
  nextRandom = proc () -> do
    (st, pr, gen) <- getA -< ()
    let (r, gen') = random gen
    putA -< (st, pr, gen')
    returnA -< r

instance L.Eval M Val  where
  lookup = lookup
  boolLit = Concrete.boolLit
  and = Concrete.and
  or = Concrete.or
  not = Concrete.not
  numLit = Concrete.numLit
  randomNum = Concrete.randomNum
  add = Concrete.add
  sub = Concrete.sub
  mul = Concrete.mul
  div = Concrete.div
  eq = Concrete.eq
  fixEval = Concrete.fixEval

instance L.Run M Val where
  store = store
  if_ = Concrete.if_