{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module GrammarSemantics where

import           SharedSemantics hiding (all)
import           Signature hiding (Top)
import           Syntax hiding (Fail)
import           Utils

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
import           Control.Category hiding ((.))

import           Data.Abstract.UncertainResult
import           Data.Constructor
import           Data.Foldable (foldr')
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as LM
import           Data.Hashable
import qualified Data.Map as M
import           Data.Order
import           Data.Term hiding (wildcard)
import           Data.TermEnv

import           TreeAutomata

type Term = Grammar

newtype TermEnv = TermEnv (HashMap TermVar Term) deriving (Show, Eq, Hashable)
newtype Interp a b = Interp (Reader (StratEnv, Int, Alphabet) (State TermEnv (Uncertain (->))) a b)
  deriving (Arrow, ArrowApply, ArrowChoice, ArrowDeduplicate, ArrowPlus, ArrowZero, Category, Complete, PreOrd)

runInterp :: Interp a b -> Int -> Alphabet -> StratEnv -> TermEnv -> a -> UncertainResult (TermEnv, b)
runInterp (Interp f) i alph senv tenv a = runUncertain (runState (runReader f)) (tenv, ((senv, i, alph), a))

eval :: Int -> Strat -> Alphabet -> StratEnv -> TermEnv -> Term -> UncertainResult (TermEnv, Term)
eval i s = runInterp (eval' s) i

-- Create grammars -----------------------------------------------------------------------------------

sortToName :: Sort -> Name
sortToName sort = case sort of
  Sort (SortId name) -> name
  _ -> error "Parametric polymorphism is not yet supported"

toRhs :: (Constructor,Fun) -> Rhs
toRhs (Constructor constr, Fun sorts _) = Ctor constr (map sortToName sorts)

toProd :: (Sort, [(Constructor,Fun)]) -> (Name, [Rhs])
toProd (sort, rhs) = (sortToName sort, map toRhs rhs)

createGrammar :: Signature -> Grammar
createGrammar (Signature (_, sorts) _) = Grammar start prods
  where
    start = "Start"
    startProd = (start, map (Eps . sortToName) (LM.keys sorts))
    builtins = [("String", [ Ctor "String" []]), ("INT", [ Ctor "INT" []])]
    prods = M.fromList $ startProd : map toProd (LM.toList sorts) ++ builtins

sigToAlphabet :: Signature -> Alphabet
sigToAlphabet (Signature (_, sorts) _) = M.fromList alph where
  alph = map (\(c,v) -> (sortToName c,length v)) $ LM.toList sorts

-- Instances -----------------------------------------------------------------------------------------
deriving instance ArrowReader (StratEnv, Int, Alphabet) Interp
deriving instance ArrowState TermEnv Interp

instance Complete z => ArrowTry x y z Interp where
  tryA (Interp f) (Interp g) (Interp h) = Interp (tryA f g h)

instance PreOrd Term where
  (⊑) = subsetOf

instance Complete Term where
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

instance ArrowFix' Interp Term where
  -- TODO: this should be rewritten to use the fixpoint caching algorithm.
  fixA' f z = proc x -> do
    i <- getFuel -< ()
    if i <= 0
    then top -< ()
    else do
      (env,_,alph) <- askA -< ()
      localFuel (f (fixA' f) z) -< ((env,i-1,alph),x)
    where
      getFuel = Interp (askA >>^ (\(_,b,_) -> b))
      localFuel (Interp g) = Interp $ proc ((env,i,alph),a) -> localA g -< ((env,i,alph),a)

instance UpperBounded (Interp () Term) where
  top = proc () -> do
    (_,_,alph) <- askA -< ()
    success ⊔ failA' -< wildcard alph

instance ArrowFail () Interp where
  failA = Interp failA

instance HasStratEnv Interp where
  readStratEnv = Interp (const () ^>> askA >>^ (\(a,_,_) -> a))
  localStratEnv senv f = proc a -> do
    (_,i,alph) <- askA -< ()
    r <- localA f -< ((senv,i,alph),a)
    returnA -< r

instance IsTerm Term Interp where
  matchTermAgainstConstructor matchSubterms = proc (Constructor c,ts,t) -> do
    let (Grammar s ps) = epsilonClosure t
      in case M.lookup s ps of
      Just rhss -> do
        let gs = [ Grammar nt ps | Ctor c' ts' <- rhss, c == c', eqLength ts ts', nt <- ts' ]
        _ <- matchSubterms -< (ts, gs)
        returnA -< t
      Nothing -> failA -< ()

  matchTermAgainstExplode matchCons matchSubterms = undefined

  matchTermAgainstNumber = proc (_,t) -> do
    let (Grammar s ps) = epsilonClosure t
      in case M.lookup s ps of
      Just rhss -> if elem (Ctor "INT" []) rhss
        then returnA -< t
        else failA -< ()
      Nothing -> failA -< ()

  matchTermAgainstString = proc (_,t) -> do
    let (Grammar s ps) = epsilonClosure t
      in case M.lookup s ps of
      Just rhss -> if elem (Ctor "String" []) rhss
        then returnA -< t
        else failA -< ()
      Nothing -> failA -< ()

  equal = proc (t1,t2) -> if t1 == t2
    then returnA -< t1
    else failA -< ()

  convertFromList = undefined

  mapSubterms f = proc t -> do
    ts' <- f -< permutate t
    returnA -< union' ts'

  cons = proc (Constructor c,ts) -> let start = uniqueStart () in if null ts
    then returnA -< Grammar start $ M.fromList [(start, [ Ctor c [] ])]
    else let ss = [ s | Grammar s _ <- ts ]
             (Grammar _ ps) = union' ts
         in returnA -< Grammar start $ M.insertWith (++) start [ Ctor c ss ] ps

  numberLiteral = proc _ -> returnA -< numberGrammar
  stringLiteral = proc _ -> returnA -< stringGrammar

instance IsTermEnv TermEnv Term Interp where
  getTermEnv = getA
  putTermEnv = putA
  lookupTermVar f g = proc (v,TermEnv env) ->
    case LM.lookup v env of
      Just t -> f -< t
      Nothing ->
        (proc () -> do
            t <- top -< ()
            putTermEnv -< TermEnv (LM.insert v t env)
            f -< t)
        ⊔ g
        -<< ()
  insertTerm = arr $ \(v,t,TermEnv env) -> TermEnv (LM.insert v t env)
  deleteTermVars = arr $ \(vars,TermEnv env) -> TermEnv (foldr' LM.delete env vars)
  unionTermEnvs = arr (\(vars,TermEnv e1,TermEnv e2) -> TermEnv (LM.union e1 (foldr' LM.delete e2 vars)))

-- Helpers -------------------------------------------------------------------------------------------
dom :: HashMap TermVar t -> [TermVar]
dom = LM.keys

stringGrammar :: Term
stringGrammar = Grammar start prods where
  start = uniqueStart ()
  prods = M.fromList [(start, [ Eps "String" ])
                     ,("String", [ Ctor "String" []])]

numberGrammar :: Term
numberGrammar = Grammar start prods where
  start = uniqueStart ()
  prods = M.fromList [(start, [ Eps "INT" ])
                     ,("INT", [ Ctor "INT" []])]
