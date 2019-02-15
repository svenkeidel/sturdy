{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures -fno-warn-orphans #-}
module SortSemantics where

import           Prelude hiding ((.),fail)
-- import qualified Prelude as P

import qualified ConcreteSemantics as C
import           SharedSemantics as Shared
import           Sort
import           SortContext (Context,Signature(..))
import qualified SortContext as Ctx
-- import           Soundness
import           Syntax hiding (Fail,TermPattern(..))
import           Utils

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Const
import           Control.Arrow.Deduplicate
import           Control.Arrow.Except
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.Reader
import           Control.Arrow.State
import           Control.Arrow.Trans
import           Control.Arrow.Abstract.Join
import           Control.Arrow.Transformer.Const
import           Control.Arrow.Transformer.Reader
import           Control.Arrow.Transformer.State
import           Control.Arrow.Transformer.Abstract.Completion
import           Control.Arrow.Transformer.Abstract.Except
import           Control.Arrow.Transformer.Abstract.Error
import           Control.Arrow.Transformer.Abstract.Fix
import           Control.Arrow.Transformer.Abstract.Terminating
import           Control.DeepSeq

import           Data.Abstract.FreeCompletion hiding (Top)
import qualified Data.Abstract.FreeCompletion as Free
import           Data.Abstract.Except as E
import           Data.Abstract.Error as F
import qualified Data.Abstract.Maybe as A
import           Data.Abstract.DiscretePowerset(Pow)
import qualified Data.Abstract.DiscretePowerset as P
-- import qualified Data.Concrete.Powerset as C
-- import qualified Data.Concrete.Error as CE
-- import qualified Data.Concrete.Failure as CF
import           Data.Abstract.Map (Map)
import qualified Data.Abstract.Map as S
import qualified Data.Abstract.StackWidening as SW
import           Data.Abstract.Terminating (Terminating)
import qualified Data.Abstract.Terminating as T
import           Data.Abstract.Widening as W
import           Data.Foldable (foldr')
-- import           Data.GaloisConnection
-- import qualified Data.HashMap.Lazy as M
import           Data.Hashable
import           Data.Order
import           Data.Profunctor
import           Data.Lens(Prism')
import qualified Data.Lens as L

-- import           Test.QuickCheck hiding (Success)
import           Text.Printf
import           GHC.Exts(IsString(..))

data Term = Term { sort :: Sort, context :: Context }

type TermEnv = Map TermVar Term

type TypeError = Pow String

type Interp s x y =
  Fix (Strat,Term) Term
    (SortT  
      (ConstT Context
       (ReaderT StratEnv
        (StateT TermEnv
         (ExceptT ()
          (ErrorT TypeError
           (CompletionT
            (TerminatingT
             (FixT s () ()
              (->)))))))))) x y

runInterp :: forall x y. Interp _ x y -> Int -> Int -> StratEnv -> Context -> TermEnv -> x -> Terminating (FreeCompletion (Error (Pow String) (Except () (TermEnv,y))))
runInterp f k l senv ctx tenv a =
  runFixT stackWidening (T.widening resultWidening)
   (runTerminatingT
    (runCompletionT
     (runErrorT
      (runExceptT
       (runStateT
        (runReaderT
         (runConstT ctx
           (runSortT f
           ))))))))
    (tenv, (senv, a))
  where
    stackWidening :: SW.StackWidening _ (TermEnv, (StratEnv, (Strat, Term)))
    stackWidening = SW.filter' (L.second (L.second (L.first stratCall)))
                  $ SW.groupBy (\(_,(_,((stratVar,_,_),_))) -> stratVar)
                  $ SW.project (L.second (L._2 . L._2))
                  $ SW.stack
                  $ SW.reuseFirst
                  $ SW.maxSize k
                  $ topWidening

    stratCall :: Prism' Strat (StratVar,[Strat],[TermVar])
    stratCall = L.prism' (\(sv,sa,ta) -> Call sv sa ta)
                    (\s -> case s of
                       Call sv sa ta -> Just (sv,sa,ta)
                       _ -> Nothing)

    resultWidening :: Widening (FreeCompletion (Error TypeError (Except () (TermEnv,Term))))
    resultWidening = Free.widening (F.widening P.widening (E.widening (\_ _ -> (Stable,())) (S.widening termWidening W.** termWidening)))

    topWidening :: SW.StackWidening SW.Stack (TermEnv,Term)
    topWidening = SW.topOut'' $ \(env,_) -> (S.map (const (Term Top ctx)) env,Term Top ctx)

    termWidening :: Widening Term
    termWidening (Term s _) (Term s' _) = let ~(st,s'') = Sort.widening l s s' in (st,Term s'' ctx)

    -- from' :: (TermEnv, (StratEnv, (Strat, Term))) -> ((Strat, StratEnv), (TermEnv, Term))
    -- from' (tenv',(senv',(s,t))) = ((s,senv'),(tenv',t))
                                  
    -- to' :: ((Strat, StratEnv), (TermEnv, Term)) -> (TermEnv, (StratEnv, (Strat, Term)))
    -- to' ((s,senv'),(tenv',t)) = (tenv',(senv',(s,t)))

eval :: Int -> Int -> Strat -> StratEnv -> Context -> TermEnv -> Term -> Terminating (FreeCompletion (Error TypeError (Except () (TermEnv,Term))))
eval i j s = runInterp (Shared.eval' s) i j

-- Instances -----------------------------------------------------------------------------------------
type instance Fix x y (SortT c) = SortT (Fix x y c)
newtype SortT c x y = SortT { runSortT :: c x y }
  deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowExcept e,ArrowReader r,ArrowState s,ArrowFail e,ArrowJoin,ArrowConst ctx)

instance ArrowReader StratEnv c => HasStratEnv (SortT c) where
  readStratEnv = proc _ -> ask -< ()
  {-# INLINE readStratEnv #-}
  localStratEnv senv f = proc a ->
    local f -< (senv,a)
  {-# INLINE localStratEnv #-}

instance (ArrowChoice c, ArrowApply c, ArrowJoin c, ArrowConst Context c, ArrowFail e c, ArrowExcept () c, IsString e)
    => IsTerm Term (SortT c) where
  matchTermAgainstConstructor matchSubterms = proc (c,ps,t@(Term s ctx)) ->
    case (c,ps,s) of
      ("Cons",[_,_],List a) ->
          (do
           ss <- matchSubterms -< (ps,[Term a ctx,Term (List a) ctx])
           cons -< ("Cons",ss))
           <⊔>
           (throw -< ())
      ("Cons",[_,_],_) | isList t -> (do
           ss <- matchSubterms -< (ps,[Term Top ctx,Term (List Top) ctx])
           cons -< ("Cons",ss))
           <⊔>
           (throw -< ())
      ("Cons",_,_) -> typeMismatch -< ("List",show s)
      ("Nil",[],_) -> (returnA -< Term (List Bottom) ctx) <⊔> (throw -< ())
      ("Nil",_,_) -> throw -< ()
      ("",_,Tuple ss)
        | eqLength ss ps -> do
          ss' <- matchSubterms -< (ps,sortsToTerms ss ctx)
          cons -< (c,ss')
        | otherwise -> throw -< ()
      ("",_,_) -> typeMismatch -< ("Tuple",show s)
      (_,_,Top) -> do
         (| joinList (typeError -< printf "cannot find constructor %s in context" (show c))
                     (\(Signature ss _) ->
                       if eqLength ss ps
                       then do
                         ss' <- matchSubterms -< (ps,sortsToTerms ss ctx)
                         cons -< (c,ss')
                       else throw -< ()) |)
            (Ctx.lookupCons ctx c)
          <⊔>
          do throw -< ()
      _ -> do
        (| joinList (typeError -< printf "cannot find constructor %s in context" (show c))
                    (\(c',Signature ss _) ->
                       if c == c' && eqLength ss ps
                       then do
                         ss' <- matchSubterms -< (ps,sortsToTerms ss ctx)
                         cons -< (c',ss')
                       else throw -< ()) |)
           (Ctx.lookupSort ctx s)

  matchTermAgainstString = proc (_,t) ->
    if isLexical t
      then (returnA -< t) <⊔> (throw -< ())
      else throw -< ()

  matchTermAgainstNumber = proc (_,t) ->
    if isNumeric t
      then (returnA -< t) <⊔> (throw -< ())
      else throw -< ()

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
    t | sort t == Bottom -> throw -< ()
      | isSingleton t1 && isSingleton t2 -> returnA -< t
      | otherwise -> (returnA -< t) <⊔> (throw -< ())

  convertFromList = proc (t,ts) ->
    if isLexical t && isList ts
      then (returnA -< Term Top (context t)) <⊔> (throw -< ()) -- cannot deduct target sort from sort Lexical
      else throw -< ()

  mapSubterms f = proc s -> do
    ctx <- askConst -< ()
    (| joinList
      (throw -< ())
      (\(c,ts) -> do
        ts' <- f -< ts
        cons -< (c,ts')) |)
      (map (\(c',Signature ss _) -> (c',sortsToTerms ss ctx)) (Ctx.lookupSort ctx (sort s)))

  cons = proc (c, ss) -> do
    ctx <- askConst -< ()
    returnA -< case c of
      "Cons" -> case ss of
        [Term a _,Term (List b) _] -> Term (List a) ctx ⊔ Term (List b) ctx
        _ -> Term Top ctx
      "Nil" -> case ss of
        [] -> Term (List Bottom) ctx
        _ -> Term Top ctx
      "" -> Term (Tuple (map sort ss)) ctx
      _ -> glb (Term Top ctx : [ Term s ctx | Signature ss' s <- Ctx.lookupCons ctx c, ss ⊑ sortsToTerms ss' ctx ])

  numberLiteral = proc _ -> do
    ctx <- askConst -< ()
    returnA -< Term Numerical ctx

  stringLiteral = proc _ -> do
    ctx <- askConst -< ()
    returnA -< Term Lexical ctx

instance ArrowTrans SortT where
  type Dom SortT x y = x
  type Cod SortT x y = y
  lift = SortT
  {-# INLINE lift #-}
  unlift = runSortT
  {-# INLINE unlift #-}

instance (ArrowApply c,Profunctor c) => ArrowApply (SortT c) where
  app = SortT (lmap (first unlift) app)
  {-# INLINE app #-}

instance ArrowFix x y c => ArrowFix x y (SortT c) where
  fix = liftFix
  {-# INLINE fix #-}

deriving instance ArrowDeduplicate x y c => ArrowDeduplicate x y (SortT c)

instance Complete (FreeCompletion Term) where
  Lower x ⊔ Lower y = Lower (x ⊔ y)
  _ ⊔ _ = Free.Top

instance Complete (FreeCompletion TermEnv) where
  Lower x ⊔ Lower y = Lower (x ⊔ y)
  _ ⊔ _ = Free.Top

instance (ArrowChoice c, ArrowJoin c, ArrowState TermEnv c) => IsTermEnv TermEnv Term (SortT c) where
  getTermEnv = get
  {-# INLINE getTermEnv #-}
  putTermEnv = put
  {-# INLINE putTermEnv #-}
  lookupTermVar f g = proc (v,env,ex) ->
    case S.lookup v env of
      A.Just t        -> f -< t
      A.Nothing       -> g -< ex
      A.JustNothing t -> (f -< t) <⊔> (g -< ex)
  {-# INLINE lookupTermVar #-}
  insertTerm = arr $ \(v,t,env) -> S.insert v t env
  {-# INLINE insertTerm #-}
  deleteTermVars = arr $ \(vars,env) -> foldr' S.delete env vars
  {-# INLINE deleteTermVars #-}
  unionTermEnvs = arr (\(vars,e1,e2) -> S.union e1 (foldr' S.delete e2 vars))
  {-# INLINE unionTermEnvs #-}


-- alphaTerm :: Context -> C.Pow C.Term -> Term
-- alphaTerm ctx = lub . fmap (toSort ctx)

-- alphaEnv :: Context -> C.Pow C.TermEnv -> TermEnv
-- alphaEnv ctx = lub . fmap (\(C.TermEnv e) -> S.fromList (M.toList (fmap (toSort ctx) e)))

-- alphaErr :: (Complete e', Complete x') => (e -> e') -> (x -> x') -> C.Pow (CF.Failure String (CE.Error e x)) -> Failure String (Error e' x')
-- alphaErr f g = lub . fmap (\er -> case er of
--   CF.Fail msg -> F.Fail msg
--   CF.Success (CE.Fail x) -> F.Success (E.Fail (f x))
--   CF.Success (CE.Success x) -> F.Success (E.Success (g x)))

-- instance Soundness (StratEnv,Context) (Interp (SW.Categories (Strat,StratEnv) (TermEnv,Term) SW.Stack)) where
--  sound (senv,ctx) xs f g = forAll (choose (0,3)) $ \i -> forAll (choose (3,5)) $ \j -> 
--    let con :: Terminating (FreeCompletion (Failure String (Error () (TermEnv,_))))
--        con = Terminating (Lower (
--                  alphaErr P.id ((alphaEnv ctx . return) *** alphaSing) (fmap (\(x,tenv) -> C.runInterp f senv tenv x) xs)))
--        abst :: Terminating (FreeCompletion (Failure String (Error () (TermEnv,_))))
--        abst = runInterp g i j senv ctx (alphaEnv ctx (fmap snd xs)) (alpha (fmap fst xs))
--    in counterexample (printf "%s ⊑/ %s" (show con) (show abst)) $ con ⊑ abst

instance Show Term where
  show (Term s _) = show s

instance Eq Term where
  Term t1 _ == Term t2 _ = t1 == t2

instance Hashable Term where
  hashWithSalt salt (Term s _) = {-# SCC "Term.hash" #-} salt `hashWithSalt` s

instance NFData Term where
  rnf = rnf . sort

instance PreOrd Term where
  Term s1 ctx ⊑ Term s2 _ = {-# SCC "Term.lt" #-} Ctx.subtype ctx s1 s2

instance Complete Term where
  Term t1 ctx ⊔ Term t2 _ = {-# SCC "Term.join" #-} Term (Ctx.lub ctx t1 t2) ctx

instance CoComplete Term where
  Term t1 ctx ⊓ Term t2 _ = Term (Ctx.glb ctx t1 t2) ctx

matchTerm :: Term -> [t'] -> Interp s ([Sort],Sort) (Term, ([t'], [Term]))
matchTerm (Term termSort ctx) ts = proc (patParams,patSort) ->
  if eqLength patParams ts && Term patSort ctx ⊑ Term termSort ctx
    then returnA -< (Term patSort ctx, (ts,map (\s -> Term s ctx) patParams))
    else throw -< ()

buildTerm :: Context -> [Term] -> Interp s ([Sort],Sort) Term
buildTerm ctx ts = proc (cParams,cSort) ->
  if eqLength cParams ts && Term (Tuple $ map sort ts) ctx  ⊑ Term (Tuple cParams) ctx
    then returnA -< Term cSort ctx
    else returnA -< Term Top ctx

convertToList :: [Term] -> Context -> Term
convertToList [] ctx = Term (List Bottom) ctx
convertToList ts ctx = Term (List (sort $ lub ts)) ctx

toSort :: Context -> C.Term -> Term
toSort ctx t = case t of
  C.StringLiteral _ -> Term Lexical ctx
  C.NumberLiteral _ -> Term Numerical ctx
  C.Cons c ts -> glb $ Term Top ctx : [ Term s ctx | Signature ss s <- Ctx.lookupCons ctx c, map (toSort ctx) ts ⊑ sortsToTerms ss ctx]

-- sortContext :: C.Term -> Context -> Context
-- sortContext t ctx = let Term s _ = (toSort ctx t) in _
  -- Context
  -- { signatures = case term of
  --     C.Cons c ts -> unionsWith (\s1 s2 -> nub (s1 ++ s2)) (M.singleton c [(map termToSort ts, termToSort term)] : map (signatures . sortContext) ts)
  --     _ -> M.empty
  -- , lexicals = Set.empty
  -- , injectionClosure = case term of
  --     C.Cons _ ts -> unionsWith Set.union $ M.singleton (termToSort term) (Set.fromList (map termToSort ts)) : map (injectionClosure . sortContext) ts
  --     _ -> M.empty
  -- }

isLexical :: Term -> Bool
isLexical (Term s ctx) = Ctx.isLexical ctx s

isNumeric :: Term -> Bool
isNumeric (Term s ctx) = Ctx.isNumerical ctx s

isList :: Term -> Bool
isList (Term s ctx) = Ctx.isList ctx s

isSingleton :: Term -> Bool
isSingleton (Term s ctx) = Ctx.isSingleton ctx s

sortsToTerms :: [Sort] -> Context -> [Term]
sortsToTerms ss ctx = map (`Term` ctx) ss

typeMismatch :: (ArrowFail e c, IsString e) => c (String,String) a
typeMismatch = lmap (\(expected,actual) -> printf "expected type %s but got type %s" (show expected) (show actual)) typeError

typeError :: (ArrowFail e c, IsString e) => c String a
typeError = lmap fromString fail
