{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures -fno-warn-orphans #-}
module SortSemantics where

import           Prelude hiding ((.),fail)
import qualified Prelude as P

import qualified ConcreteSemantics as C
import           SharedSemantics
import           Signature hiding (Top,Sort,List,Tuple,Option,Bottom)
import qualified Sort as Sort
import           Soundness
import           Syntax hiding (Fail,TermPattern(..))
import           Utils

import           Control.Arrow
import           Control.Arrow.Const
import           Control.Arrow.Deduplicate
import           Control.Arrow.Except
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.Reader
import           Control.Arrow.State
import           Control.Arrow.Transformer.Abstract.Completion
import           Control.Arrow.Transformer.Abstract.Fixpoint
import           Control.Arrow.Transformer.Abstract.HandleExcept
import           Control.Arrow.Transformer.Const
import           Control.Arrow.Transformer.Reader
import           Control.Arrow.Transformer.State
import           Control.Category
import           Control.Monad (zipWithM)

import           Data.Abstract.FreeCompletion hiding (Top)
import qualified Data.Abstract.FreeCompletion as F
import           Data.Abstract.HandleError hiding (fromMaybe)
import           Data.Abstract.HandleError as E hiding (fromMaybe)
import qualified Data.Abstract.Maybe as A
import qualified Data.Concrete.Powerset as C
import           Data.Abstract.PreciseStore (Store)
import qualified Data.Abstract.PreciseStore as S
import qualified Data.Abstract.StackWidening as SW
import           Data.Abstract.Terminating
import           Data.Abstract.Widening as W
import           Data.Constructor
import           Data.Foldable (foldr')
import           Data.GaloisConnection
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.Hashable
import           Data.List (intercalate,foldl',nub)
import           Data.Maybe
import           Data.Monoidal
import           Data.Order
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.String (IsString,fromString)
import           Data.Text (pack,unpack)

import           GHC.Generics (Generic)

import           Test.QuickCheck hiding (Success)
import           Text.Printf

-- TODO: perhaps reuse the Sort module?
data Sort = Bottom | Top | Lexical | Numerical | Option Sort | List Sort | Tuple [Sort] | Sort Sort.SortId
  deriving (Eq, Ord, Generic)
instance IsString Sort where
  fromString s = Sort (Sort.SortId (pack s))

data SortContext = SortContext {
  signatures :: HashMap Constructor [([Sort], Sort)]
, lexicals :: Set Sort
, injectionClosure :: HashMap Sort (Set Sort)
} deriving (Eq,Show)

instance PreOrd SortContext where
  (⊑) = undefined -- TODO
instance Complete SortContext where
  (⊔) = union

union :: SortContext -> SortContext -> SortContext
union s1 s2 = SortContext
  { signatures = M.unionWith (\ss1 ss2 -> nub $ ss1 ++ ss2) (signatures s1) (signatures s2)
  , lexicals = Set.union (lexicals s1) (lexicals s2)
  , injectionClosure = M.unionWith Set.union (injectionClosure s1) (injectionClosure s2)
  }

lookupCons :: Constructor -> SortContext -> [([Sort], Sort)]
lookupCons c ctx = fromMaybe [] (M.lookup c (signatures ctx))

lookupSort :: Sort -> SortContext -> [(Constructor, [Sort])]
lookupSort s ctx = [ (c,ss') | (c,ss) <- M.toList (signatures ctx), (ss',s') <- ss, Term s' ctx ⊑ Term s ctx || s' == s ]

data Term = Term {
  sort :: Sort
, context :: SortContext
} deriving (Eq)

sortsToTerms :: [Sort] -> SortContext -> [Term]
sortsToTerms ss ctx = map (\s -> Term s ctx) ss

isLexical :: Term -> Bool
isLexical (Term Lexical _) = True
isLexical (Term s ctx) = Set.member s (lexicals ctx)

isList :: Term -> Bool
isList (Term (List _) _) = True
isList _ = False

isSingleton :: Term -> Bool
isSingleton (Term s ctx) = case s of
  Bottom -> True
  Lexical -> False
  Numerical -> False
  Sort (Sort.SortId c) -> length (maybeToList (M.lookup (Constructor c) (signatures ctx))) == 1
  Option s' -> isSingleton (Term s' ctx)
  List s' -> isSingleton (Term s' ctx)
  Tuple ss -> Prelude.all (\s' -> isSingleton (Term s' ctx)) ss
  Top -> False

type TermEnv = Store TermVar Term
instance UpperBounded TermEnv where
  top = S.empty

-- TODO: perhaps use HasSignature?
newtype Interp s a b =
  Interp (
   Fix (Strat,Term) Term
    (Const SortContext
     (Reader StratEnv
      (State TermEnv
       (Except ()
        (Completion
         (Fixpoint s () ()
          (->))))))) a b)

runInterp :: Interp (SW.Categories (Strat,StratEnv) (TermEnv,Term) SW.Stack) a b -> Int -> StratEnv -> SortContext -> TermEnv -> a -> Terminating (FreeCompletion (Error () (TermEnv,b)))
runInterp (Interp f) k senv sig tenv a =
  runFix' stackWidening sortWidening
    (runCompletion
     (runExcept
      (runState
       (runReader
        (runConst sig f)))))
    (tenv, (senv, a))
  where
    stackWidening :: SW.StackWidening (SW.Categories (Strat,StratEnv) (TermEnv, Term) SW.Stack) (TermEnv, (StratEnv, (Strat, Term)))
    stackWidening = SW.categorize (Iso from' to') (SW.stack (SW.maxSize k SW.topOut))
    sortWidening :: Widening (FreeCompletion (Error () (TermEnv,Term)))
    sortWidening = F.widening (E.widening (\_ _ -> ()) (S.widening W.finite W.** W.finite))

from' :: (TermEnv, (StratEnv, (Strat, Term))) -> ((Strat, StratEnv), (TermEnv, Term))
from' (tenv,(senv,(s,t))) = ((s,senv),(tenv,t))

to' :: ((Strat, StratEnv), (TermEnv, Term)) -> (TermEnv, (StratEnv, (Strat, Term)))
to' ((s,senv),(tenv,t)) = (tenv,(senv,(s,t)))

eval :: Int -> Strat -> StratEnv -> SortContext -> TermEnv -> Term -> Terminating (FreeCompletion (Error () (TermEnv,Term)))
eval i s = runInterp (eval' s) i

-- Instances -----------------------------------------------------------------------------------------
deriving instance Category (Interp s)
deriving instance Arrow (Interp s)
deriving instance ArrowChoice (Interp s)
deriving instance ArrowConst SortContext (Interp s)
deriving instance ArrowDeduplicate Term Term (Interp s)
deriving instance (Complete (FreeCompletion y), PreOrd y) => ArrowExcept x y () (Interp s)
deriving instance ArrowFail () (Interp s)
deriving instance ArrowFix (Strat,Term) Term (Interp s)
deriving instance ArrowReader StratEnv (Interp s)
deriving instance ArrowState TermEnv (Interp s)
deriving instance PreOrd b => PreOrd (Interp s a b)
deriving instance (Complete (FreeCompletion b), PreOrd b) => Complete (Interp s a b)
deriving instance PreOrd b => LowerBounded (Interp s a b)

instance Complete (FreeCompletion Term) where
  Lower x ⊔ Lower y = Lower (x ⊔ y)
  _ ⊔ _ = F.Top

instance Complete (FreeCompletion TermEnv) where
  Lower x ⊔ Lower y = Lower (x ⊔ y)
  _ ⊔ _ = F.Top

instance (PreOrd x, Complete (FreeCompletion x)) => Complete (FreeCompletion [x]) where
  Lower xs ⊔ Lower ys | eqLength xs ys = zipWithM (\x y -> Lower x ⊔ Lower y) xs ys
  _ ⊔ _ = F.Top

instance ArrowApply (Interp s) where
  app = Interp $ (\(Interp f, b) -> (f,b)) ^>> app

instance HasStratEnv (Interp s) where
  readStratEnv = proc _ ->
    ask -< ()
  localStratEnv senv f = proc a ->
    local f -< (senv,a)

instance IsTermEnv TermEnv Term (Interp s) where
  getTermEnv = get
  putTermEnv = put
  lookupTermVar f g = proc (v,env) ->
    case S.lookup v env of
      A.Just t -> f -< t
      A.JustNothing t -> joined f g -< (t,())
      A.Nothing -> g -< ()
  insertTerm = arr $ \(v,t,env) -> S.insert v t env
  deleteTermVars = arr $ \(vars,env) -> foldr' S.delete env vars
  unionTermEnvs = arr (\(vars,e1,e2) -> S.union e1 (foldr' S.delete e2 vars))

instance IsTerm Term (Interp s) where
  matchTermAgainstConstructor matchSubterms = proc (c,ps,Term s _) -> do
    ctx <- askConst -< ()
    case c of
      "Cons" -> case (ps,s) of
        ([_,_],List a) -> (do
           ss <- matchSubterms -< (ps,[Term a ctx,Term (List a) ctx])
           cons -< ("Cons",ss))
           ⊔ (fail -< ())
        _ -> fail -< ()
      "Nil" -> if Term s ctx ⊑ Term (List Top) ctx
        then (returnA -< Term s ctx) ⊔ (fail -< ())
        else (returnA -< Term Top ctx) ⊔ (fail -< ())
      _ ->
        lubA (proc (c',ss) -> if c == c' && eqLength ss ps
               then do
                 ss' <- matchSubterms -< (ps,ss)
                 cons -< (c,ss')
               else fail -< ()) -<< map (\(c',ss) -> (c',sortsToTerms ss ctx)) (lookupSort s ctx)

  matchTermAgainstString = proc (_,t) ->
    if isLexical t
      then returnA ⊔ fail' -< t
      else fail -< ()

  matchTermAgainstNumber = proc (_,t@(Term termSort _)) -> case termSort of
    Numerical -> returnA ⊔ fail' -< t
    _ -> fail -< ()

  matchTermAgainstExplode matchCons matchSubterms = proc t -> case t of
    _ | isLexical t -> do
      matchSubterms -< convertToList [] (context t)
      returnA -< t
    Term Numerical ctx -> do
      matchSubterms -< convertToList [] ctx
      returnA -< t
    Term _ ctx -> do
      matchCons -< Term Lexical ctx
      matchSubterms -< Term (List Top) ctx
      returnA -< t

  equal = proc (t1,t2) -> case t1 ⊓ t2 of
    t | sort t == Bottom -> fail -< ()
      | isSingleton t1 && isSingleton t2 -> returnA -< t
      | otherwise -> returnA ⊔ fail' -< t

  convertFromList = proc (t,ts) ->
    if isLexical t && isList ts
      then returnA ⊔ fail' -< Term Top (context t) -- cannot deduct target sort from sort Lexical
      else fail -< ()

  mapSubterms f = proc s -> do
    ctx <- askConst -< ()
    lubA (proc (c,ts) -> do
             ts' <- f -< ts
             cons -< (c,ts')) -< map (\(c',ss) -> (c',sortsToTerms ss ctx)) (lookupSort (sort s) ctx)

  cons = proc (c, ss) -> do
    ctx <- askConst -< ()
    returnA -< case c of
      "Cons" -> case ss of
        [Term a _,Term (List b) _] -> Term (List a) ctx ⊔ Term (List b) ctx
        _ -> Term Top ctx
      "Nil" -> case ss of
        [] -> Term (List Bottom) ctx
        _ -> Term Top ctx
      _ -> glb (Term Top ctx : [ Term s ctx | (ss', s) <- lookupCons c ctx, ss ⊑ sortsToTerms ss' ctx ])

  numberLiteral = proc _ -> do
    ctx <- askConst -< ()
    returnA -< Term Numerical ctx

  stringLiteral = proc _ -> do
    ctx <- askConst -< ()
    returnA -< Term Lexical ctx

instance Galois (C.Pow C.Term) SortContext where
  alpha = lub . fmap sortContext
  gamma = error "Infinite"

instance Galois (C.Pow C.Term) Term where
  alpha pow = lub $ fmap (\t -> Term { sort = termToSort t, context = alpha pow }) pow
  gamma = error "Infinite"

instance Galois (C.Pow C.TermEnv) TermEnv where
  alpha = lub . fmap (\(C.TermEnv e) -> S.fromList (M.toList (fmap alphaSing e)))
  gamma = undefined

instance Soundness (StratEnv,SortContext) (Interp (SW.Categories (Strat,StratEnv) (TermEnv,Term) SW.Stack)) where
 sound (senv,ctx) xs f g = forAll (choose (0,3)) $ \i ->
   let con :: Terminating (FreeCompletion (Error () (TermEnv,_)))
       con = Terminating (Lower (alpha (fmap (\(x,tenv) -> C.runInterp f senv tenv x) xs)))
       abst :: Terminating (FreeCompletion (Error () (TermEnv,_)))
       abst = runInterp g i senv ctx (alpha (fmap snd xs)) (alpha (fmap fst xs))
   in counterexample (printf "%s ⊑/ %s" (show con) (show abst)) $ con ⊑ abst

instance Show Sort where
  show x = case x of
    Bottom -> "Bottom"
    Top -> "Top"
    Lexical -> "LEX"
    Numerical -> "NUM"
    Option s -> "Option (" ++ show s ++ ")"
    List s -> "List (" ++ show s ++ ")"
    Tuple ss -> "Tuple (" ++ intercalate ", " (map show ss) ++ ")"
    Sort (SortId s) -> unpack s

instance Hashable Sort

instance Show Term where
  show (Term s _) = show s

instance Hashable Term where
  hashWithSalt salt (Term s _) = salt `hashWithSalt` s

instance UpperBounded Term where
  top = Term Top (SortContext M.empty Set.empty M.empty)

instance PreOrd Term where
  Term Bottom _ ⊑ _ = True
  _ ⊑ Term Top _ = True

  Term Lexical _ ⊑ t2 = isLexical t2
  Term (Option s1) ctx1 ⊑ Term (Option s2) ctx2 = Term s1 ctx1 ⊑ Term s2 ctx2
  Term (List s1) ctx1 ⊑ Term (List s2) ctx2 = Term s1 ctx1 ⊑ Term s2 ctx2
  Term (Tuple ss1) ctx1 ⊑ Term (Tuple ss2) ctx2 = length ss1 == length ss2 &&
    P.all (\(s1,s2) -> Term s1 ctx1 ⊑ Term s2 ctx2) (zip ss1 ss2)

  Term s1 _ ⊑ Term s2 _ | s1 == s2 = True
  Term s1 ctx ⊑ Term s2 _ = case M.lookup s1 (injectionClosure ctx) of
    Just s -> Set.member s2 s
    Nothing -> False

instance Complete Term where
  t1 ⊔ t2 | t1 ⊑ t2 = t2
          | t2 ⊑ t1 = t1
          | otherwise = Term Top (context t1)

instance CoComplete Term where
  t1 ⊓ t2 | t1 ⊑ t2 = t1
          | t2 ⊑ t1 = t2
          | otherwise = Term Bottom (context t1)

matchTerm :: Term -> [t'] -> Interp s ([Sort],Sort) (Term, ([t'], [Term]))
matchTerm (Term termSort ctx) ts = proc (patParams,patSort) ->
  if eqLength patParams ts && Term patSort ctx ⊑ Term termSort ctx
    then returnA -< (Term patSort ctx, (ts,map (\s -> Term s ctx) patParams))
    else fail -< ()

buildTerm :: SortContext -> [Term] -> Interp s ([Sort],Sort) Term
buildTerm ctx ts = proc (cParams,cSort) ->
  if eqLength cParams ts && Term (Tuple $ map sort ts) ctx  ⊑ Term (Tuple cParams) ctx
    then returnA -< Term cSort ctx
    else returnA -< Term Top ctx

convertToList :: [Term] -> SortContext -> Term
convertToList [] ctx = Term (List Bottom) ctx
convertToList ts ctx = Term (List (sort $ lub ts)) ctx

termToSort :: C.Term -> Sort
termToSort (C.Cons _ _) = Sort "Exp"
termToSort (C.StringLiteral _) = Lexical
termToSort (C.NumberLiteral _) = Numerical

sortContext :: C.Term -> SortContext
sortContext term = SortContext
  { signatures = case term of
      C.Cons c ts -> unionsWith (++) tss where
        tss = M.singleton c [(map termToSort ts, termToSort term)]
            : map (signatures . sortContext) ts
      _ -> M.empty
  , lexicals = Set.empty
  -- Top can be formed from any sort.
  , injectionClosure = M.insertWith Set.union Top (Set.fromList [termToSort term,Numerical,Lexical]) $ case term of
      C.Cons _ ts ->
        unionsWith Set.union $ M.singleton (termToSort term) (Set.fromList (map termToSort ts))
                             : map (injectionClosure . sortContext) ts
      _ -> M.empty
  }

-- | The union of a list of maps, with a combining operation.
unionsWith :: (Hashable k, Ord k) => (a->a->a) -> [HashMap k a] -> HashMap k a
unionsWith f xs = foldl' (M.unionWith f) M.empty xs
