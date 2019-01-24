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
import           SortContext(Context,Signature(..),Sort(Sort),SortId(..),sorts)
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
import           Control.Arrow.Transformer.Abstract.Except
import           Control.Arrow.Transformer.Abstract.Failure
import           Control.Arrow.Transformer.Reader
import           Control.Arrow.Transformer.State
import           Control.Category hiding ((.))

import           Data.Abstract.FreeCompletion (FreeCompletion(Lower,Top))
import qualified Data.Abstract.FreeCompletion as Free
import           Data.Abstract.Error (Error)
import qualified Data.Abstract.Error as E
import           Data.Abstract.Failure (Failure)
import qualified Data.Abstract.Failure as F
import           Data.Abstract.Maybe
import           Data.Abstract.Map (Map)
import qualified Data.Abstract.Map as S
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
import           Data.Monoidal
import           Data.Order
import           Data.Term
import           Data.Text (Text)

import           TreeAutomata

import qualified Test.QuickCheck as Q
import           Text.Printf

data Constr = Constr Text | StringLit Text | NumLit Int deriving (Eq, Ord, Show)
newtype Term = Term (GrammarBuilder Constr) deriving (Complete, Eq, Hashable, PreOrd, Show)

type TermEnv = Map TermVar Term

newtype Interp s a b =
  Interp (
    Fix (Strat,Term) Term
      (ReaderT StratEnv
        (StateT TermEnv
          (ExceptT ()
            (FailureT String
              (CompletionT
                (FixT s () ()
                  (->))))))) a b)

runInterp :: Interp (SW.Categories (Strat,StratEnv) (TermEnv, Term) SW.Stack) a b -> Int -> StratEnv -> TermEnv -> a -> Terminating (FreeCompletion (Failure String (Error () (TermEnv, b))))
runInterp (Interp f) i senv tenv a =
  runFixT' stackWidening grammarWidening
    (runCompletionT
      (runFailureT
        (runExceptT
          (runStateT
            (runReaderT f)))))
    (tenv, (senv, a))
  where
    stackWidening :: SW.StackWidening (SW.Categories (Strat,StratEnv) (TermEnv, Term) SW.Stack) (TermEnv, (StratEnv, (Strat, Term)))
    stackWidening = SW.categorize (Iso (\(te,(se,(s,t))) -> ((s,se),(te,t))) (\((s,se),(te,t)) -> (te,(se,(s,t)))))
      (SW.stack
       (SW.maxSize i
        (SW.reuse bestChoice
         (SW.fromWidening (S.widening widening W.** widening)))))
    grammarWidening = Free.widening (F.widening (E.widening (\_ _ -> ()) (S.widening widening W.** widening)))

bestChoice :: (TermEnv, Term) -> [(TermEnv, Term)] -> (TermEnv, Term)
bestChoice e [] = e
bestChoice _ (x:_) = x

eval :: Int -> Strat -> StratEnv -> TermEnv -> Term -> Terminating (FreeCompletion (Failure String (Error () (TermEnv, Term))))
eval i s = runInterp (eval' s) i

-- Create grammars -----------------------------------------------------------------------------------

sortToNonterm :: Sort -> Nonterm
sortToNonterm sort = case sort of
  Sort (SortId nt) -> nt
  _ -> error "Parametric polymorphism is not yet supported"

toRhs :: (Constructor,Signature) -> Rhs Constr
toRhs (Constructor constr, Signature ss _) = Ctor (Constr constr) (map sortToNonterm ss)

toProd :: (Sort, [(Constructor,Signature)]) -> (Nonterm, [Rhs Constr])
toProd (sort, rhss) = (sortToNonterm sort, map toRhs rhss)

createGrammar :: Context -> GrammarBuilder Constr
createGrammar ctx = grammar startSymbol prods
  where
    startSymbol = "Start"
    startProd = (startSymbol, map (Eps . sortToNonterm) (LM.keys (sorts ctx)))
    -- TODO: what to do with these builtins?
    builtins = [("String", [ Ctor (Constr "String") []]) ]
    prods = M.fromList $ startProd : map toProd (LM.toList (sorts ctx)) ++ builtins

-- Instances -----------------------------------------------------------------------------------------
deriving instance Category (Interp s)
deriving instance Arrow (Interp s)
deriving instance ArrowChoice (Interp s)
deriving instance ArrowReader StratEnv (Interp s)
deriving instance ArrowState TermEnv (Interp s)
deriving instance ArrowFail String (Interp s)
deriving instance ArrowExcept () (Interp s)
deriving instance PreOrd b => PreOrd (Interp s a b)
deriving instance (Complete (FreeCompletion b), PreOrd b) => Complete (Interp s a b)
deriving instance PreOrd b => LowerBounded (Interp s a b)
deriving instance ArrowFix (Strat,Term) Term (Interp s)

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

instance CoComplete (GrammarBuilder Constr) where
  (⊓) = intersection

instance ArrowApply (Interp s) where
  app = Interp $ (\(Interp f, b) -> (f,b)) ^>> app

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
  matchTermAgainstConstructor matchSubterms = proc (Constructor c,ps,Term g) ->
    lubA (proc (c',ts) -> case c' of
             Constr c'' | c'' == c && eqLength ps ts -> do
               ts' <- matchSubterms -< (ps,ts)
               cons -< (Constructor c,ts')
             _ -> throw -< ()) -<< mapSnd (map Term) (toSubterms g)

  matchTermAgainstExplode _ _ = undefined

  matchTermAgainstNumber = proc (n,Term g) -> matchLit -< (normalize (epsilonClosure g), NumLit n)
  matchTermAgainstString = proc (s,Term g) -> matchLit -< (normalize (epsilonClosure g), StringLit s)

  equal = proc (Term g1, Term g2) -> case g1 ⊓ g2 of
    g | isEmpty g -> throw -< ()
      | isSingleton (normalize (epsilonClosure g1)) && isSingleton (normalize (epsilonClosure g2)) -> returnA -< Term g
      | otherwise -> (returnA -< Term g) ⊔ (throw -< ())

  convertFromList = undefined

  mapSubterms f = proc (Term g) ->
    lubA (proc (c,ts) -> case c of
             Constr c' -> do
               ts' <- f -< ts
               cons -< (Constructor c',ts')
             _ -> throw -< ()) -< mapSnd toTerms (toSubterms g)

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
  lookupTermVar f g = proc (v,env,ex) ->
    case S.lookup v env of
      Just t -> f -< t
      JustNothing t -> joined f g -< (t,ex)
      Nothing -> g -< ex
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

instance Soundness StratEnv (Interp (SW.Categories (Strat,StratEnv) (TermEnv, Term) SW.Stack)) where
  sound senv xs f g = Q.forAll (Q.choose (2,3)) $ \i ->
    let con :: FreeCompletion (Failure String (Error () (TermEnv,_)))
        con = Lower (alpha (fmap (\(x,tenv) -> C.runInterp f senv tenv x) xs))
        abst :: FreeCompletion (Failure String (Error () (TermEnv,_)))
        -- TODO: using fromTerminating is a bit of a hack...
        abst = fromTerminating Top $ runInterp g i senv (alpha (fmap snd xs)) (alpha (fmap fst xs))
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

checkConstructorAndLength :: Constructor -> [t'] -> Interp s (Constr, [GrammarBuilder Constr]) (Constructor, ([t'], [Term]))
checkConstructorAndLength (Constructor c) ts = proc (c', gs) -> case c' of
  Constr c'' | c == c'' && eqLength ts gs -> returnA -< (Constructor c, (ts, toTerms gs))
  _ -> throw -< ()

matchLit :: Interp s (GrammarBuilder Constr, Constr) Term
-- TODO: check if production to n has empty argument list? This should be the case by design.
matchLit = proc (g,l) -> case g `produces` l of
  True | isSingleton g -> returnA -< Term g
       | otherwise -> (returnA -< Term g) ⊔ (throw -< ())
  False -> throw -< ()

stringGrammar :: Text -> Term
stringGrammar s = Term (singleton (StringLit s))

numberGrammar :: Int -> Term
numberGrammar n = Term (singleton (NumLit n))
