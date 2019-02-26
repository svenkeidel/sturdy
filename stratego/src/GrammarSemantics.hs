{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures -fno-warn-orphans #-}
module GrammarSemantics where

import           Prelude hiding (fail)

import qualified ConcreteSemantics as C
import           SharedSemantics
import           SortContext(Context,Signature(..),Sort(Sort),SortId(..),sorts)
import           Syntax hiding (Fail)
import           Utils

import           Control.Category hiding ((.))
import           Control.Arrow
import           Control.Arrow.Deduplicate
import           Control.Arrow.Except
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.Reader
import           Control.Arrow.State
import           Control.Arrow.Const
import           Control.Arrow.Abstract.Join
import           Control.Arrow.Transformer.Reader
import           Control.Arrow.Transformer.State
import           Control.Arrow.Transformer.Abstract.Completion
import           Control.Arrow.Transformer.Abstract.Except
import           Control.Arrow.Transformer.Abstract.Error
import           Control.Arrow.Transformer.Abstract.Fix
import           Control.Arrow.Transformer.Abstract.Terminating

import           Data.Coerce
import           Data.Identifiable
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
import           Data.Profunctor
import qualified Data.Lens as L
import           Data.Abstract.FreeCompletion (FreeCompletion(Lower,Top))
import qualified Data.Abstract.FreeCompletion as Free
import           Data.Abstract.Except (Except)
import qualified Data.Abstract.Except as E
import           Data.Abstract.Error (Error)
import qualified Data.Abstract.Error as F
import qualified Data.Abstract.Maybe as AM
import           Data.Abstract.Map (Map)
import qualified Data.Abstract.Map as S
import qualified Data.Abstract.StackWidening as SW
import           Data.Abstract.Terminating (Terminating,fromTerminating)
import qualified Data.Abstract.Terminating as T
import           Data.Abstract.Widening as W
import           Data.Abstract.DiscretePowerset(Pow)

import qualified Test.QuickCheck as Q
import           Text.Printf

import           TreeAutomata

data Constr = Constr Text | StringLit Text | NumLit Int deriving (Eq, Ord, Show)
newtype Term = Term (GrammarBuilder Constr) deriving (Complete, Eq, Hashable, PreOrd, Show)

type TermEnv = Map TermVar Term

type Stack = SW.Groups StratVar SW.Stack

type Interp s a b =
  Fix (Strat,Term) Term
    (GrammarT
      (ReaderT StratEnv
        (StateT TermEnv
          (ExceptT ()
            (ErrorT (Pow String)
              (CompletionT
                (TerminatingT
                  (FixT s () ()
                    (->))))))))) a b

runInterp :: Interp _ a b -> Int -> StratEnv -> TermEnv -> a -> Terminating (FreeCompletion (Error (Pow String) (Except () (TermEnv, b))))
runInterp f i senv0 tenv0 a =
  runFixT stackWidening (T.widening grammarWidening)
    (runTerminatingT
      (runCompletionT
        (runErrorT
          (runExceptT
            (runStateT
              (runReaderT
                (runGrammarT f)))))))
    (tenv0, (senv0, a))
  where
    stackWidening :: SW.StackWidening _ (TermEnv, (StratEnv, (Strat, Term)))
    stackWidening = SW.filter' (L.second (L.second (L.first stratCall)))
                  $ SW.groupBy (L.iso' (\(tenv,(senv,(strat,term))) -> ((strat,senv),(term,tenv))) (\((strat,senv),(term,tenv)) -> (tenv,(senv,(strat,term)))))
                  $ SW.stack
                  $ SW.reuseFirst
                  $ SW.maxSize i
                  $ SW.fromWidening (widening W.** S.widening widening)

    grammarWidening = Free.widening (F.widening W.finite (E.widening (\_ _ -> (Stable,())) (S.widening widening W.** widening)))

eval :: Int -> Strat -> StratEnv -> TermEnv -> Term -> Terminating (FreeCompletion (Error (Pow String) (Except () (TermEnv, Term))))
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
type instance Fix x y (GrammarT c) = GrammarT (Fix x y c)
newtype GrammarT c x y = GrammarT { runGrammarT :: c x y }
  deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowExcept e,ArrowReader r,ArrowState s,ArrowFail e,ArrowJoin,ArrowConst ctx)
instance (Profunctor c, ArrowApply c) => ArrowApply (GrammarT c) where app = GrammarT $ lmap (first runGrammarT) app

deriving instance PreOrd (c x y) => PreOrd (GrammarT c x y)
deriving instance Complete (c x y) => Complete (GrammarT c x y)
deriving instance LowerBounded (c x y) => LowerBounded (GrammarT c x y)
deriving instance ArrowFix x y c => ArrowFix x y (GrammarT c)

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

instance (Arrow c, Profunctor c) => ArrowDeduplicate Term Term (GrammarT c) where
  -- We normalize and determinize here to reduce duplicated production
  -- rules, which can save us up to X times the work.
  dedup f = rmap (Term . determinize . normalize . fromTerm) f

instance (Arrow c, Profunctor c, ArrowReader StratEnv c) => HasStratEnv (GrammarT c) where
  readStratEnv = lmap (const ()) ask
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

instance (ArrowJoin c, ArrowExcept () c, ArrowChoice c, Profunctor c, LowerBounded (c () Term)) => IsTerm Term (GrammarT c) where
  matchTermAgainstConstructor matchSubterms = proc (Constructor c,ps,Term g) ->
    (| joinList (bottom -< ()) (\(c',ts) -> case c' of
         Constr c'' | c'' == c && eqLength ps ts -> do
           ts' <- matchSubterms -< (ps,ts)
           cons -< (Constructor c,ts')
         _ -> throw -< ()) |) (coerce (toSubterms g))

  matchTermAgainstExplode _ _ = error "unsupported"

  matchTermAgainstNumber = proc (n,Term g) -> matchLit -< (normalize (epsilonClosure g), NumLit n)
  matchTermAgainstString = proc (s,Term g) -> matchLit -< (normalize (epsilonClosure g), StringLit s)

  equal = proc (Term g1, Term g2) -> case g1 ⊓ g2 of
    g | isEmpty g -> throw -< ()
      | isSingleton (normalize (epsilonClosure g1)) && isSingleton (normalize (epsilonClosure g2)) -> returnA -< Term g
      | otherwise -> (returnA -< Term g) <⊔> (throw -< ())

  convertFromList = error "unsupported"


  mapSubterms f = proc (Term g) ->
    (| joinList (bottom -< ()) (\(c,ts) -> case c of
             Constr c' -> do
               ts' <- f -< ts
               cons -< (Constructor c',ts')
             _ -> throw -< ()) |) (coerce (toSubterms g))

  cons = arr $ \(Constructor c,ts) -> Term (normalize (epsilonClosure (addConstructor (Constr c) (fromTerms ts))))
  numberLiteral = arr numberGrammar
  stringLiteral = arr stringGrammar

instance TermUtils Term where
  convertToList ts = case ts of
    (x:xs) -> Term (addConstructor (Constr "Cons") [fromTerm x, fromTerm (convertToList xs)])
    [] -> Term (singleton (Constr "Nil"))
  size (Term g) = TreeAutomata.size g
  height (Term g) = TreeAutomata.height g

instance (ArrowChoice c, ArrowJoin c, Profunctor c, ArrowState TermEnv c) => IsTermEnv TermEnv Term (GrammarT c) where
  getTermEnv = get
  putTermEnv = put
  lookupTermVar f g = proc (v,env,ex) ->
    case S.lookup v env of
      AM.Just t -> f -< t
      AM.JustNothing t -> (f -< t) <⊔> (g -< ex)
      AM.Nothing -> g -< ex
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

sound :: (Identifiable b1, Show b2, Complete b2, Galois (C.Pow a1) a2, Galois (C.Pow b1) b2)
      => StratEnv -> C.Pow (a1, C.TermEnv) -> C.Interp a1 b1 -> Interp _ a2 b2 -> Q.Property
sound senv xs f g = Q.forAll (Q.choose (2,3)) $ \i ->
  let con :: FreeCompletion (Error (Pow String) (Except () (TermEnv,_)))
      con = Lower (alpha (fmap (\(x,tenv) -> C.runInterp f senv tenv x) xs))
      abst :: FreeCompletion (Error (Pow String) (Except () (TermEnv,_)))
      -- TODO: using fromTerminating is a bit of a hack...
      abst = fromTerminating Top $ runInterp g i senv (alpha (fmap snd xs)) (alpha (fmap fst xs))
  in Q.counterexample (printf "%s ⊑/ %s" (show con) (show abst)) $ con ⊑ abst

-- Helpers -------------------------------------------------------------------------------------------
widening :: Widening Term
widening = undefined
-- widening (Term t1) (Term t2) = Term (widen t1 t2)

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

matchLit :: (ArrowChoice c, ArrowJoin c, ArrowExcept () c) => c (GrammarBuilder Constr, Constr) Term
-- TODO: check if production to n has empty argument list? This should be the case by design.
matchLit = proc (g,l) -> case g `produces` l of
  True | isSingleton g -> returnA -< Term g
       | otherwise -> (returnA -< Term g) <⊔> (throw -< ())
  False -> throw -< ()

stringGrammar :: Text -> Term
stringGrammar s = Term (singleton (StringLit s))

numberGrammar :: Int -> Term
numberGrammar n = Term (singleton (NumLit n))
