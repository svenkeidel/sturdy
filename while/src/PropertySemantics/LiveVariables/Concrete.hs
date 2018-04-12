{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Props.LiveVariables.Concrete where

import Prelude (String, ($), (.), fmap,fst)

import WhileLanguage (HasStore(..), HasProp(..), Statement, Label)
import qualified WhileLanguage as L

import Vals.Concrete.Val
import qualified Vals.Concrete.Semantics as Concrete

import Props.LiveVariables.Prop

import Data.Error
import Data.Text (Text)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Utils

import Control.Arrow
import Control.Arrow.Fail
import Control.Arrow.Fix
import Control.Arrow.State
import Control.Arrow.Utils

import System.Random

lookup :: (ArrowChoice c, ArrowFail String c, HasStore c Store, HasProp c CProp) => c (Text,Label) Val
lookup = modifyProp (arr $ \((x,_),LiveVars maybe must) ->
           -- `x` is live at all assignments `maybe(x)`, hence remove `x` from `maybe` and it to `live`
           LiveVars (Map.delete x maybe)
                    (Map.insertWith Set.union x (lookupM x maybe) must))
     &&> Concrete.lookup

store :: (ArrowChoice c, HasStore c Store, HasProp c CProp) => c (Text,Val,Label) ()
store = modifyProp (arr $ \((x,_,l),LiveVars maybe must) ->
          -- overwrite `maybe(x)={l}`, meaning that `x` was not live at any of the previous assignments `maybe(x)`
          LiveVars (Map.insert x (Set.singleton l) maybe) must)
    &&> Concrete.store


----------
-- Arrows
----------

type State = (Store,CProp,StdGen)
initState :: State
initState = (initStore, initCProp, mkStdGen 0)

type In a = (State,a)
type Out a = Error String (State,a)
type M = StateArrow State (ErrorArrow String (Fix (In [Statement]) (Out ())))

runM :: [Statement] -> Error String (State,())
runM ss = runFix (runErrorArrow (runStateArrow L.run)) (initState, ss)

run :: [Statement] -> Error String (Store,CProp)
run = fmap ((\(st,pr,_) -> (st,pr)) . fst) . runM

runLifted :: [Statement] -> Error String (LiftedStore,FCProp)
runLifted = fmap (liftStore *** finalizeCProp) . run

instance L.HasStore M Store where
  getStore = getA >>> arr (\(st, _, _) -> st)
  putStore = modifyA $ arr $ \(st,(_,pr,rnd)) -> (st,pr,rnd)

instance L.HasProp M CProp where
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