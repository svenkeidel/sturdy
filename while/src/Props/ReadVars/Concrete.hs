{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Props.ReadVars.Concrete where

import Prelude hiding (lookup,and,or,not,div)

import WhileLanguage

import Vals.Concrete.Val
import Vals.Concrete.Semantics (State)
import qualified Vals.Concrete.Semantics as Concrete

import Props.FailedReads.Prop

import Data.Error
import qualified Data.Set as Set

import Control.Arrow
import Control.Arrow.Fail
import Control.Arrow.Fix
import Control.Arrow.State
import Control.Arrow.Property

import System.Random

----------
-- Arrows
----------

type Interp = PropertyArrow CProp Concrete.Interp

runInterp :: [Statement] -> Error String (State,CProp)
runInterp ss =
  second fst <$>
    runFix (runErrorArrow (runStateArrow (runProperty run)))
      ((initStore, mkStdGen 0), (initCProp, ss))

instance HasStore Interp Store where
  getStore = getA >>> arr fst
  putStore = modifyA $ arr $ \(st,(_,rnd)) -> (st,rnd)

instance HasRandomGen Interp where
  nextRandom = proc () -> do
    (st, gen) <- getA -< ()
    let (r, gen') = random gen
    putA -< (st, gen')
    returnA -< r

instance Eval Interp Val  where
  lookup = proc (v,x) -> do
    modifyProp (arr (uncurry Set.insert)) -< v
    liftProperty lookup -< (v,x)
  boolLit = liftProperty boolLit
  and = liftProperty and
  or = liftProperty or
  not = liftProperty not
  numLit = liftProperty numLit
  randomNum = liftProperty randomNum
  add = liftProperty add
  sub = liftProperty sub
  mul = liftProperty mul
  div = liftProperty div
  eq = liftProperty eq
  fixEval f = _

instance Run Interp Val where
  store = liftProperty store
  if_ f g = liftProperty _
