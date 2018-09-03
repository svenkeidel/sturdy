{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures -fno-warn-orphans #-}
module GrammarSemantics where

import           Prelude hiding (fail,Just,Nothing)

import qualified ConcreteSemantics as C
import           SharedSemantics
import           Signature hiding (Top)
import           Soundness
import           Syntax hiding (Fail)
import           Utils

import           Control.Monad (zipWithM)
import           Control.Arrow
import           Control.Arrow.Deduplicate
import           Control.Arrow.Except
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.Reader
import           Control.Arrow.State
import           Control.Arrow.Transformer.Abstract.Completion
import           Control.Arrow.Transformer.Abstract.Fixpoint
import           Control.Arrow.Transformer.Abstract.HandleExcept
import           Control.Arrow.Transformer.Reader
import           Control.Arrow.Transformer.State
import           Control.Category hiding ((.))

import           Data.Abstract.FreeCompletion (FreeCompletion(Lower,Top))
import qualified Data.Abstract.FreeCompletion as F
import           Data.Abstract.HandleError (Error)
import qualified Data.Abstract.HandleError as E
import           Data.Abstract.Maybe
import           Data.Abstract.PreciseStore (Store)
import qualified Data.Abstract.PreciseStore as S
import qualified Data.Abstract.StackWidening as SW
import           Data.Abstract.Terminating (Terminating,fromTerminating)
import           Data.Abstract.Widening as W
import qualified Data.Concrete.Powerset as C
import           Data.Constructor
import           Data.Foldable (foldr')
import           Data.GaloisConnection
import qualified Data.HashMap.Lazy as LM
import           Data.Hashable
import qualified Data.Map as M
import           Data.Order
import           Data.Term
import           Data.Text (Text)

import           TreeAutomata

import qualified Test.QuickCheck as Q
import           Text.Printf

data Constr = Constr Text | StringLit Text | NumLit Int deriving (Eq, Ord, Show)
newtype Term = Term (GrammarBuilder Constr) deriving (Complete, Eq, Hashable, PreOrd, Show)

type TermEnv = Store TermVar Term

newtype Interp s a b =
  Interp (
    Fix Strat Term
      (Reader StratEnv
        (State TermEnv
          (Except ()
            (Completion
              (Fixpoint s () ()
                (->)))))) a b)

runInterp :: Interp SW.Stack a b -> Int -> Alphabet Constr -> StratEnv -> TermEnv -> a -> Terminating (FreeCompletion (Error () (TermEnv, b)))
runInterp (Interp f) i alph senv tenv a =
  -- Term (wildcard alph)
  runFix' (SW.stack (SW.maxSize i SW.topOut)) grammarWidening
    (runCompletion
      (runExcept
        (runState
          (runReader f))))
    (tenv, (senv, a))
  where
    grammarWidening = (F.widening (E.widening (\_ _ -> ()) (S.widening widening W.** widening)))

eval :: Int -> Strat -> Alphabet Constr -> StratEnv -> TermEnv -> Term -> Terminating (FreeCompletion (Error () (TermEnv, Term)))
eval i s = runInterp (eval' s) i

-- Create grammars -----------------------------------------------------------------------------------

sortToNonterm :: Sort -> Nonterm
sortToNonterm sort = case sort of
  Sort (SortId nt) -> nt
  _ -> error "Parametric polymorphism is not yet supported"

toRhs :: (Constructor,Fun) -> Rhs Constr
toRhs (Constructor constr, Fun sorts _) = Ctor (Constr constr) (map sortToNonterm sorts)

toProd :: (Sort, [(Constructor,Fun)]) -> (Nonterm, [Rhs Constr])
toProd (sort, rhss) = (sortToNonterm sort, map toRhs rhss)

createGrammar :: Signature -> GrammarBuilder Constr
createGrammar (Signature (_, sorts) _) = grammar startSymbol prods
  where
    startSymbol = "Start"
    startProd = (startSymbol, map (Eps . sortToNonterm) (LM.keys sorts))
    -- TODO: what to do with these builtins?
    builtins = [("String", [ Ctor (Constr "String") []]) ]
    prods = M.fromList $ startProd : map toProd (LM.toList sorts) ++ builtins

-- Instances -----------------------------------------------------------------------------------------
deriving instance Category (Interp s)
deriving instance Arrow (Interp s)
deriving instance ArrowChoice (Interp s)
deriving instance ArrowReader StratEnv (Interp s)
deriving instance ArrowState TermEnv (Interp s)
deriving instance ArrowFail () (Interp s)
deriving instance ArrowFix Strat Term (Interp s)
deriving instance (Complete (FreeCompletion y), PreOrd y) => ArrowExcept x y () (Interp s)
deriving instance PreOrd b => PreOrd (Interp s a b)
deriving instance (Complete (FreeCompletion b), PreOrd b) => Complete (Interp s a b)
deriving instance PreOrd b => LowerBounded (Interp s a b)

-- TODO: what requires these instances?
instance Hashable Closure where
  hashWithSalt _ _ = undefined

instance UpperBounded TermEnv where
  top = S.empty

instance PreOrd StratEnv where
  (⊑) = undefined

instance UpperBounded StratEnv where
  top = LM.empty

instance PreOrd Strat where
  (⊑) = undefined

instance UpperBounded Strat where
  top = undefined

instance Hashable Constr where
  hashWithSalt s (Constr c) = s `hashWithSalt` (0::Int) `hashWithSalt` c
  hashWithSalt s (StringLit s') = s `hashWithSalt` (1::Int) `hashWithSalt` s'
  hashWithSalt s (NumLit n) = s `hashWithSalt` (2::Int) `hashWithSalt` n

instance PreOrd (GrammarBuilder Constr) where
  g1 ⊑ g2 = d1 `subsetOf` d2 where
    g1' = normalize (epsilonClosure g1)
    g2' = normalize (epsilonClosure g2)
    d1 = if isDeterministic g1' then g1' else determinize g1'
    d2 = if isDeterministic g2' then g2' else determinize g2'

instance Complete (GrammarBuilder Constr) where
  (⊔) = union

instance ArrowApply (Interp s) where
  app = Interp $ (\(Interp f, b) -> (f,b)) ^>> app

-- TODO: is this correct?
instance ArrowFix (Strat,Term) Term (Interp s) where
  fix f = f (fix f)

instance ArrowDeduplicate Term Term (Interp s) where
  -- We normalize and determinize here to reduce duplicated production
  -- rules, which can save us up to X times the work.
  dedup f = Term . determinize . normalize . fromTerm ^<< f

instance HasStratEnv (Interp s) where
  readStratEnv = Interp (const () ^>> ask)
  localStratEnv senv f = proc a -> local f -< (senv,a)

instance Complete (FreeCompletion Term) where
  Lower x ⊔ Lower y = Lower (x ⊔ y)
  _ ⊔ _ = Top

instance Complete (FreeCompletion TermEnv) where
  Lower x ⊔ Lower y = Lower (x ⊔ y)
  _ ⊔ _ = Top

instance Complete (FreeCompletion (GrammarBuilder Constr)) where
  Lower x ⊔ Lower y = Lower (x ⊔ y)
  _ ⊔ _ = Top

instance (PreOrd x, Complete (FreeCompletion x)) => Complete (FreeCompletion [x]) where
  Lower xs ⊔ Lower ys | eqLength xs ys = zipWithM (\x y -> Lower x ⊔ Lower y) xs ys
  _ ⊔ _ = Top

instance IsTerm Term (Interp s) where
  matchTermAgainstConstructor matchSubterms = proc (Constructor c,ts,Term g) ->
    lubA (reconstruct <<< second matchSubterms <<< checkConstructorAndLength c ts) -<< toSubterms g

  matchTermAgainstExplode _ _ = undefined

  matchTermAgainstNumber = proc (n,Term g) -> matchLit -< (normalize (epsilonClosure g), NumLit n)
  matchTermAgainstString = proc (s,Term g) -> matchLit -< (normalize (epsilonClosure g), StringLit s)

  equal = proc (Term g1, Term g2) -> case intersection g1 g2 of
    g | isEmpty g -> fail -< ()
      | isSingleton (normalize (epsilonClosure g1)) && isSingleton (normalize (epsilonClosure g2)) -> returnA -< Term g
      | otherwise -> returnA ⊔ fail' -< Term g

  convertFromList = undefined

  mapSubterms f = proc (Term g) -> do
    let subterms = mapSnd toTerms (toSubterms g)
    Term ^<< lubA (fromSubterms . return ^<< second (fromTerms ^<< f)) -< subterms

  cons = proc (Constructor c,ts) -> returnA -< Term (normalize (epsilonClosure (addConstructor (Constr c) (fromTerms ts))))
  numberLiteral = arr numberGrammar
  stringLiteral = arr stringGrammar

instance TermUtils Term where
  convertToList ts = case ts of
    (x:xs) -> Term (addConstructor (Constr "Cons") [fromTerm x, fromTerm (convertToList xs)])
    [] -> Term (singleton (Constr "Nil"))
  size (Term g) = TreeAutomata.size g
  height (Term g) = TreeAutomata.height g

instance IsTermEnv TermEnv Term (Interp s) where
  getTermEnv = get
  putTermEnv = put
  lookupTermVar f g = proc (v,env) ->
    case S.lookup v env of
      Just t -> f -< t
      JustNothing t -> joined f g -< (t,())
      Nothing -> g -< ()
  insertTerm = arr $ \(v,t,env) -> S.insert v t env
  deleteTermVars = arr $ \(vars,env) -> foldr' S.delete env vars
  unionTermEnvs = arr (\(vars,e1,e2) -> S.union e1 (foldr' S.delete e2 vars))

instance Galois (C.Pow C.Term) (GrammarBuilder Constr) where
  alpha = lub . fmap go
    where
      go (C.Cons (Constructor c) ts) = addConstructor (Constr c) (fmap go ts)
      go (C.StringLiteral s) = fromTerm (stringGrammar s)
      go (C.NumberLiteral n) = fromTerm (numberGrammar n)
  gamma = error "Uncomputable"

instance Galois (C.Pow C.Term) Term where
  alpha = Term . alpha
  gamma = error "Uncomputable"

instance Galois (C.Pow C.TermEnv) TermEnv where
  alpha = lub . fmap (\(C.TermEnv e) -> S.fromList (LM.toList (fmap alphaSing e)))
  gamma = undefined

instance Soundness (StratEnv, Alphabet Constr) (Interp SW.Stack) where
  sound (senv,alph) xs f g = Q.forAll (Q.choose (2,3)) $ \i ->
    let con :: FreeCompletion (Error () (TermEnv,_))
        con = Lower (alpha (fmap (\(x,tenv) -> C.runInterp f senv tenv x) xs))
        abst :: FreeCompletion (Error () (TermEnv,_))
        -- TODO: using fromTerminating is a bit of a hack...
        abst = fromTerminating Top $ runInterp g i alph senv (alpha (fmap snd xs)) (alpha (fmap fst xs))
    in Q.counterexample (printf "%s ⊑/ %s" (show con) (show abst)) $ con ⊑ abst

-- Helpers -------------------------------------------------------------------------------------------
widening :: Widening Term
widening (Term t1) (Term t2) = Term (widen t1 t2)

mapSnd :: (a -> b) -> [(c, a)] -> [(c, b)]
mapSnd _ [] = []
mapSnd f ((x,y):s) = (x,f y) : mapSnd f s

toTerms :: [GrammarBuilder Constr] -> [Term]
toTerms = map Term

fromTerms :: [Term] -> [GrammarBuilder Constr]
fromTerms = map fromTerm

fromTerm :: Term -> GrammarBuilder Constr
fromTerm (Term g) = g

reconstruct :: Interp s (Text, [Term]) Term
reconstruct = proc (c, ts) -> returnA -< (Term (fromSubterms [(Constr c, fromTerms ts)]))

checkConstructorAndLength :: Text -> [t'] -> Interp s (Constr, [GrammarBuilder Constr]) (Text, ([t'], [Term]))
checkConstructorAndLength c ts = proc (c', gs) -> case c' of
  Constr c'' | c == c'' && eqLength ts gs -> returnA -< (c, (ts, toTerms gs))
  _ -> fail -< ()

matchLit :: Interp s (GrammarBuilder Constr, Constr) Term
-- TODO: check if production to n has empty argument list? This should be the case by design.
matchLit = proc (g,l) -> case g `produces` l of
  True | isSingleton g -> returnA -< Term g
       | otherwise -> returnA ⊔ fail' -< Term g
  False -> fail -< ()

stringGrammar :: Text -> Term
stringGrammar s = Term (singleton (StringLit s))

numberGrammar :: Int -> Term
numberGrammar n = Term (singleton (NumLit n))
