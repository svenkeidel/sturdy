{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module GrammarSemantics where

import           SharedSemantics hiding (all)
import           Signature
import           Syntax hiding (Fail)

import           Control.Arrow
import           Control.Arrow.Deduplicate
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.Reader
import           Control.Arrow.State
import           Control.Arrow.Try
import           Control.Arrow.Transformer.Abstract.Uncertain
import           Control.Arrow.Transformer.Reader
import           Control.Arrow.Transformer.State
import           Control.Category

import           Data.Abstract.UncertainResult
import           Data.Constructor
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as LM
import           Data.Hashable
import qualified Data.Map as M
import           Data.Order
import           Data.Term
import           Data.TermEnv

import           TreeAutomata

newtype TermEnv = TermEnv (HashMap TermVar Grammar) deriving (Show, Eq, Hashable)
newtype Interp a b = Interp (Reader (StratEnv, Int) (State TermEnv (Uncertain (->))) a b)
  deriving (Arrow, ArrowApply, ArrowChoice, ArrowDeduplicate, ArrowPlus, ArrowZero, Category)

runInterp :: Interp a b -> Int -> StratEnv -> TermEnv -> a -> UncertainResult (TermEnv, b)
runInterp (Interp f) i senv tenv a = runUncertain (runState (runReader f)) (tenv, ((senv, i), a))

eval :: Int -> Strat -> StratEnv -> TermEnv -> Grammar -> UncertainResult (TermEnv, Grammar)
eval i s = runInterp (eval' s) i

-- Instances -----------------------------------------------------------------------------------------
deriving instance ArrowReader (StratEnv, Int) Interp
deriving instance ArrowState TermEnv Interp

instance Complete z => ArrowTry x y z Interp where
  tryA (Interp f) (Interp g) (Interp h) = Interp (tryA f g h)

instance PreOrd Grammar where
  (⊑) = subsetOf

instance Complete Grammar where
  (⊔) = union

instance PreOrd TermEnv where
  TermEnv env1 ⊑ TermEnv env2 =
    all (\v -> fromMaybe (LM.lookup v env1) ⊑ fromMaybe (LM.lookup v env2)) (dom env2)

instance Complete TermEnv where
  TermEnv env1' ⊔ TermEnv env2' = go (dom env1') env1' env2' LM.empty
    where
      go vars env1 env2 env3 = case vars of
        (v:vs) -> case (LM.lookup v env1, LM.lookup v env2) of
          (Just t1, Just t2) -> go vs env1 env2 (LM.insert v (t1 ⊔ t2) env3)
          _                  -> go vs env1 env2 env3
        [] -> TermEnv env3

instance ArrowFix' Interp Grammar where
  fixA' f = undefined

instance ArrowFail () Interp where
  failA = Interp failA

instance HasStratEnv Interp where
  readStratEnv = Interp (const () ^>> askA >>^ fst)
  localStratEnv senv f = proc a -> do
    i <- (askA >>^ snd) -< ()
    r <- localA f -< ((senv,i),a)
    returnA -< r

instance IsTerm Grammar Interp where
  matchTermAgainstConstructor matchSubterms = undefined
  matchTermAgainstExplode matchCons matchSubterms = undefined
  matchTermAgainstNumber = undefined
  matchTermAgainstString = undefined

  equal = undefined
  convertFromList = undefined
  mapSubterms f = undefined

  cons = undefined
  numberLiteral = undefined
  stringLiteral = undefined

instance IsTermEnv TermEnv Grammar Interp where
  getTermEnv = getA
  putTermEnv = putA
  lookupTermVar f g = undefined
  insertTerm = undefined
  deleteTermVars = undefined
  unionTermEnvs = undefined

-- Helpers -------------------------------------------------------------------------------------------
dom :: HashMap TermVar t -> [TermVar]
dom = LM.keys
