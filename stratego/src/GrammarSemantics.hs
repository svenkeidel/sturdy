{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module GrammarSemantics where

import           Prelude hiding (id)

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
import           Data.Text (Text)

import           TreeAutomata

data Constr = Constr Text | StringLit Text | NumLit Int deriving (Eq, Ord, Show)
newtype Term = Term (GrammarBuilder Constr) deriving (Complete, Eq, Hashable, PreOrd, Show)

newtype TermEnv = TermEnv (HashMap TermVar Term) deriving (Show, Eq, Hashable)
newtype Interp a b = Interp (Reader (StratEnv, Int, Alphabet Constr) (State TermEnv (Uncertain (->))) a b)
  deriving (Arrow, ArrowApply, ArrowChoice, ArrowDeduplicate, ArrowPlus, ArrowZero, Category, Complete, PreOrd)

runInterp :: Interp a b -> Int -> Alphabet Constr -> StratEnv -> TermEnv -> a -> UncertainResult (TermEnv, b)
runInterp (Interp f) i alph senv tenv a = runUncertain (runState (runReader f)) (tenv, ((senv, i, alph), a))

eval :: Int -> Strat -> Alphabet Constr -> StratEnv -> TermEnv -> Term -> UncertainResult (TermEnv, Term)
eval i s = runInterp (eval' s) i

-- Create grammars -----------------------------------------------------------------------------------

sortToName :: Sort -> Name
sortToName sort = case sort of
  Sort (SortId name) -> name
  _ -> error "Parametric polymorphism is not yet supported"

toRhs :: (Constructor,Fun) -> Rhs Constr
toRhs (Constructor constr, Fun sorts _) = Ctor (Constr constr) (map sortToName sorts)

toProd :: (Sort, [(Constructor,Fun)]) -> (Name, [Rhs Constr])
toProd (sort, rhss) = (sortToName sort, map toRhs rhss)

createGrammar :: Signature -> GrammarBuilder Constr
createGrammar (Signature (_, sorts) _) = grammar startSymbol prods
  where
    startSymbol = "Start"
    startProd = (startSymbol, map (Eps . sortToName) (LM.keys sorts))
    -- TODO: what to do with these builtins?
    builtins = [("String", [ Ctor (Constr "String") []]) ]
    prods = M.fromList $ startProd : map toProd (LM.toList sorts) ++ builtins

sigToAlphabet :: Signature -> Alphabet Constr
sigToAlphabet (Signature (_, sorts) _) = M.fromList alph where
  alph = map (\(c,v) -> (Constr (sortToName c),length v)) $ LM.toList sorts

-- Instances -----------------------------------------------------------------------------------------
deriving instance ArrowReader (StratEnv, Int, Alphabet Constr) Interp
deriving instance ArrowState TermEnv Interp

instance Complete z => ArrowTry x y z Interp where
  tryA (Interp f) (Interp g) (Interp h) = Interp (tryA f g h)

instance Hashable Constr where
  hashWithSalt s (Constr c) = s `hashWithSalt` (0::Int) `hashWithSalt` c
  hashWithSalt s (StringLit s') = s `hashWithSalt` (1::Int) `hashWithSalt` s'
  hashWithSalt s (NumLit n) = s `hashWithSalt` (2::Int) `hashWithSalt` n

instance PreOrd (GrammarBuilder Constr) where
  (⊑) = subsetOf

instance Complete (GrammarBuilder Constr) where
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
    success ⊔ failA' -< Term (wildcard alph)

instance PreOrd a => LowerBounded (Interp () a) where
  -- TODO: correct?
  bottom = failA

instance ArrowFail () Interp where
  failA = Interp failA

instance HasStratEnv Interp where
  readStratEnv = Interp (const () ^>> askA >>^ (\(a,_,_) -> a))
  localStratEnv senv f = proc a -> do
    (_,i,alph) <- askA -< ()
    r <- localA f -< ((senv,i,alph),a)
    returnA -< r

instance IsTerm Term Interp where
  matchTermAgainstConstructor matchSubterms = proc (Constructor c,ts,Term g) -> do
    lubA (reconstruct <<< second matchSubterms <<< checkConstructorAndLength c ts) -<< toSubterms g

  matchTermAgainstExplode matchCons matchSubterms = undefined

  matchTermAgainstNumber = proc (n,g) -> matchLit -< (g, NumLit n)
  matchTermAgainstString = proc (s,g) -> matchLit -< (g, StringLit s)

  equal = proc (Term g1, Term g2) -> case intersection g1 g2 of
    g | isEmpty g -> failA -< ()
      | otherwise -> returnA ⊔ failA' -< Term (normalize g)

  convertFromList = undefined

  mapSubterms f = proc (Term g) -> do
    g' <- lubA (fromSubterms . return ^<< second (fromTerms ^<< f)) -< [ (c, toTerms gs) | (c, gs) <- toSubterms g ]
    returnA -< Term g'

  cons = proc (Constructor c,ts) -> returnA -< Term (addConstructor (Constr c) (fromTerms ts))
  numberLiteral = arr numberGrammar
  stringLiteral = arr stringGrammar

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

toTerms :: [GrammarBuilder Constr] -> [Term]
toTerms = map Term

fromTerms :: [Term] -> [GrammarBuilder Constr]
fromTerms = map fromTerm

fromTerm :: Term -> GrammarBuilder Constr
fromTerm (Term g) = g

reconstruct :: Interp (Text, [Term]) Term
reconstruct = proc (c, ts) -> returnA -< (Term (fromSubterms [(Constr c, fromTerms ts)]))

checkConstructorAndLength :: Text -> [t'] -> Interp (Constr, [GrammarBuilder Constr]) (Text, ([t'], [Term]))
checkConstructorAndLength c ts = proc (c', gs) -> case c' of
  Constr c'' | eqLength ts gs && c == c'' -> returnA -< (c, (ts, toTerms gs))
             | otherwise -> failA -< ()
  _ -> failA -< ()

matchLit :: Interp (Term, Constr) Term
-- TODO: check if production to n has empty argument list? This should be the case by design.
matchLit = proc (Term g,l) -> case g `produces` l of
  True | isSingleton g -> returnA -< Term g
       | otherwise -> returnA ⊔ failA' -< Term g
  False -> failA -< ()

stringGrammar :: Text -> Term
stringGrammar s = Term (singleton (StringLit s))

numberGrammar :: Int -> Term
numberGrammar n = Term (singleton (NumLit n))
