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

import           Prelude hiding (id,fail)

import qualified ConcreteSemantics as C
import           SharedSemantics hiding (all,sequence)
import           Signature hiding (Top)
import           Soundness
import           Syntax hiding (Fail)
import           Utils

import           Control.Arrow
import           Control.Arrow.Deduplicate
import           Control.Arrow.Except
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.Reader
import           Control.Arrow.State
import           Control.Arrow.Transformer.Abstract.Completion
import           Control.Arrow.Transformer.Abstract.HandleExcept
import           Control.Arrow.Transformer.Reader
import           Control.Arrow.Transformer.State
import           Control.Category hiding ((.))

import           Data.Abstract.FreeCompletion
import           Data.Abstract.HandleError
import qualified Data.Concrete.Powerset as C
import           Data.Constructor
import           Data.Foldable (foldr')
import           Data.GaloisConnection
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as LM
import           Data.Hashable
import qualified Data.Map as M
import           Data.Order
import           Data.Term hiding (wildcard)
import           Data.Text (Text)

import           TreeAutomata

import qualified Test.QuickCheck as Q
import           Text.Printf

data Constr = Constr Text | StringLit Text | NumLit Int deriving (Eq, Ord, Show)
newtype Term = Term (GrammarBuilder Constr) deriving (Complete, Eq, Hashable, PreOrd, Show)

newtype TermEnv = TermEnv (HashMap TermVar Term) deriving (Show, Eq, Hashable)
newtype Interp a b = Interp (Reader (StratEnv, Int, Alphabet Constr) (State TermEnv (Except () (Completion (->)))) a b)
  deriving (Arrow, ArrowApply, ArrowChoice, Category, PreOrd)

runInterp :: Interp a b -> Int -> Alphabet Constr -> StratEnv -> TermEnv -> a -> FreeCompletion (Error () (TermEnv, b))
runInterp (Interp f) i alph senv tenv a = runCompletion (runExcept (runState (runReader f))) (tenv, ((senv, i, alph), a))

eval :: Int -> Strat -> Alphabet Constr -> StratEnv -> TermEnv -> Term -> FreeCompletion (Error () (TermEnv, Term))
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

sigToAlphabet :: Signature -> Alphabet Constr
sigToAlphabet (Signature (_, sorts) _) = M.fromList alph where
  alph = map (\(c,v) -> (Constr (sortToNonterm c),length v)) $ LM.toList sorts

-- Instances -----------------------------------------------------------------------------------------
deriving instance ArrowReader (StratEnv, Int, Alphabet Constr) Interp
deriving instance ArrowState TermEnv Interp
deriving instance ArrowFail () Interp
deriving instance (Complete (FreeCompletion y), PreOrd y) => ArrowExcept x y () Interp
deriving instance (Complete (FreeCompletion b), PreOrd b) => Complete (Interp a b)
deriving instance PreOrd b => LowerBounded (Interp a b)

instance Hashable Constr where
  hashWithSalt s (Constr c) = s `hashWithSalt` (0::Int) `hashWithSalt` c
  hashWithSalt s (StringLit s') = s `hashWithSalt` (1::Int) `hashWithSalt` s'
  hashWithSalt s (NumLit n) = s `hashWithSalt` (2::Int) `hashWithSalt` n

instance PreOrd (GrammarBuilder Constr) where
  g1 ⊑ g2 = d1 `subsetOf` d2 where
    -- TODO: fix this
    d1 = {-if isDeterministic g1 then g1 else-} determinize g1
    d2 = {-if isDeterministic g2 then g2 else-} determinize g2

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

instance ArrowFix (Strat,Term) Term Interp where
  fix f = proc x -> do
    (env,i,alph) <- ask -< ()
    if i <= 0
      then top' -< ()
      else localFuel (f (fix f)) -< ((env,i-1,alph),x)
    where
      localFuel (Interp g) = Interp $ proc ((env,i,alph),a) -> local g -< ((env,i,alph),a)

instance ArrowDeduplicate Term Term Interp where
  -- We normalize and determinize here to reduce duplicated production
  -- rules, which can save us up to X times the work.
  dedup f = Term . determinize . normalize . fromTerm ^<< f

instance HasStratEnv Interp where
  readStratEnv = Interp (const () ^>> ask >>^ (\(a,_,_) -> a))
  localStratEnv senv f = proc a -> do
    (_,i,alph) <- ask -< ()
    r <- local f -< ((senv,i,alph),a)
    returnA -< r

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
  Lower xs ⊔ Lower ys | eqLength xs ys = sequence (zipWith (\x y -> Lower x ⊔ Lower y) xs ys)
  _ ⊔ _ = Top

instance IsTerm Term Interp where
  matchTermAgainstConstructor matchSubterms = proc (Constructor c,ts,Term g) -> do
    lubA (reconstruct <<< second matchSubterms <<< checkConstructorAndLength c ts) -<< toSubterms g

  matchTermAgainstExplode _ _ = undefined

  matchTermAgainstNumber = proc (n,g) -> matchLit -< (g, NumLit n)
  matchTermAgainstString = proc (s,g) -> matchLit -< (g, StringLit s)

  equal = proc (Term g1, Term g2) -> case intersection g1 g2 of
    g | isEmpty g -> fail -< ()
      | isSingleton g1 && isSingleton g2 -> returnA -< Term g
      | otherwise -> returnA ⊔ fail' -< Term g

  convertFromList = undefined

  mapSubterms f = proc (Term g) -> do
    g' <- lubA (fromSubterms . return ^<< second (fromTerms ^<< f)) -< [ (c, toTerms gs) | (c, gs) <- toSubterms g ]
    returnA -< Term g'

  cons = proc (Constructor c,ts) -> returnA -< Term (addConstructor (Constr c) (fromTerms ts))
  numberLiteral = arr numberGrammar
  stringLiteral = arr stringGrammar

instance TermUtils Term where
  convertToList ts = case ts of
    (x:xs) -> Term (addConstructor (Constr "Cons") [fromTerm x, fromTerm (convertToList xs)])
    [] -> Term (singleton (Constr "Nil"))
  size (Term g) = TreeAutomata.size g
  height (Term g) = TreeAutomata.height g

instance IsTermEnv TermEnv Term Interp where
  getTermEnv = get
  putTermEnv = put
  lookupTermVar f g = proc (v,TermEnv env) ->
    case LM.lookup v env of
      Just t -> f -< t
      Nothing ->
        (proc () -> do
            t <- top' -< ()
            putTermEnv -< TermEnv (LM.insert v t env)
            f -< t)
        ⊔ g
        -<< ()
  insertTerm = arr $ \(v,t,TermEnv env) -> TermEnv (LM.insert v t env)
  deleteTermVars = arr $ \(vars,TermEnv env) -> TermEnv (foldr' LM.delete env vars)
  unionTermEnvs = arr (\(vars,TermEnv e1,TermEnv e2) -> TermEnv (LM.union e1 (foldr' LM.delete e2 vars)))

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
  alpha = lub . fmap (\(C.TermEnv e) -> TermEnv (fmap alphaSing e))
  gamma = undefined

instance Soundness (StratEnv, Alphabet Constr) Interp where
  sound (senv,alph) xs f g = Q.forAll (Q.choose (2,3)) $ \i ->
    let con :: FreeCompletion (Error () (TermEnv,_))
        con = Lower (alpha (fmap (\(x,tenv) -> C.runInterp f senv tenv x) xs))
        abst :: FreeCompletion (Error () (TermEnv,_))
        abst = runInterp g i alph senv (alpha (fmap snd xs)) (alpha (fmap fst xs))
    in Q.counterexample (printf "%s ⊑/ %s" (show con) (show abst)) $ con ⊑ abst

-- Helpers -------------------------------------------------------------------------------------------
dom :: HashMap TermVar t -> [TermVar]
dom = LM.keys

thrd :: (a,b,c) -> c
thrd (_,_,c) = c

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
             | otherwise -> fail -< ()
  _ -> fail -< ()

top' :: Interp () Term
top' = proc () -> returnA ⊔ fail' <<< (Term . wildcard . thrd ^<< ask) -< ()

matchLit :: Interp (Term, Constr) Term
-- TODO: check if production to n has empty argument list? This should be the case by design.
matchLit = proc (Term g,l) -> case g `produces` l of
  True | isSingleton g -> returnA -< Term g
       | otherwise -> returnA ⊔ fail' -< Term g
  False -> fail -< ()

stringGrammar :: Text -> Term
stringGrammar s = Term (singleton (StringLit s))

numberGrammar :: Int -> Term
numberGrammar n = Term (singleton (NumLit n))
