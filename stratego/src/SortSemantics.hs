{-# LANGUAGE ImplicitParams #-}
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
{-# OPTIONS_GHC -Wno-partial-type-signatures -fsimpl-tick-factor=800 #-}
module SortSemantics where

import           Prelude hiding ((.),fail)

import           GenericInterpreter as Generic
import           AbstractInterpreter
import           Abstract.TermEnvironment
import           Sort
import           SortContext (Context,Signature(..))
import qualified SortContext as Ctx
import           Syntax (Strat,LStrat,LStratEnv)
import           Utils

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Const
import           Control.Arrow.Except
import           Control.Arrow.Fail
import           Control.Arrow.Order
import           Control.Arrow.Transformer.Value
import           Control.DeepSeq

import           Data.Abstract.FreeCompletion hiding (Top)
import qualified Data.Abstract.FreeCompletion as Free
import           Data.Abstract.Except as E
import           Data.Abstract.Error as F
import           Data.Abstract.Terminating (Terminating)
import           Data.Abstract.Widening as W
import           Data.Hashable
import           Data.Order hiding (LowerBounded(..))
import           Data.Profunctor
import           Data.Label

import           Text.Printf
import           GHC.Exts(IsString(..))

data Term = Term { sort :: Sort, context :: Context }

{-# SPECIALIZE Generic.eval :: Strat -> Interp Term Term Term #-}

eval :: (?sensitivity :: Int) => Int -> LStrat -> LStratEnv -> Context -> TermEnv Term -> Term -> Terminating (FreeCompletion (Error TypeError (Except () (TermEnv Term,Term))))
eval j lstrat lsenv ctx =
  let (strat,senv) = generate $ (,) <$> lstrat <*> lsenv
  in runInterp ((Generic.eval :: Strat -> Interp Term Term Term) strat ) termWidening senv ctx
  where
    termWidening :: Widening Term
    termWidening (Term s _) (Term s' _) = let ~(st,s'') = Ctx.widening ctx j s s' in (st,Term s'' ctx)

-- Instances -----------------------------------------------------------------------------------------
instance (ArrowChoice c, ArrowApply c, ArrowComplete Term c, ArrowLowerBounded c, ArrowConst Context c, ArrowFail e c, ArrowExcept () c, IsString e)
    => IsTerm Term (ValueT Term c) where
  matchCons matchSubterms = proc (c,ps,t@(Term s ctx)) ->
    case (c,ps,s) of
      (_,_,Bottom) -> do
          ss' <- matchSubterms -< (ps,[t | _ <- ps])
          buildCons -< (c,ss')
      ("Cons",[_,_],_) | isList t -> (do
           let t' = Term (List Top) ctx ⊓ t
           ss <- matchSubterms -< (ps,[getListElem t', t'])
           buildCons -< ("Cons",ss))
           <⊔>
           (throw -< ())
      ("Cons",_,_) -> typeMismatch -< ("List",show s)
      ("Nil",[],_) | isList t -> (returnA -< Term (List Bottom) ctx) <⊔> (throw -< ())
      ("Nil",_,_) -> throw -< ()
      ("",_,Tuple ss)
        | eqLength ss ps -> do
          ss' <- matchSubterms -< (ps,sortsToTerms ss ctx)
          buildCons -< (c,ss')
        | otherwise -> throw -< ()
      ("",_,Top) -> (do
           ss <- matchSubterms -< (ps,[Term Top ctx | _ <- ps ])
           buildCons -< ("",ss))
           <⊔>
           (throw -< ())
      (_,_,Top) -> do
         (| joinList (typeError -< printf "cannot find constructor %s in context" (show c))
                     (\(Signature ss _) ->
                       if eqLength ss ps
                       then do
                         ss' <- matchSubterms -< (ps,sortsToTerms ss ctx)
                         buildCons -< (c,ss')
                       else throw -< ()) |)
            (Ctx.lookupCons ctx c)
          <⊔>
          do throw -< ()
      _ -> do
        (| joinList (bottom -< ()) -- printf "cannot find constructors of %s in context" (show s)
                    (\(c',Signature ss _) ->
                       if c == c' && eqLength ss ps
                       then do
                         ss' <- matchSubterms -< (ps,sortsToTerms ss ctx)
                         buildCons -< (c',ss')
                       else throw -< ()) |)
           (Ctx.lookupSort ctx s)

  matchString = proc (_,t) ->
    if isLexical t
      then (returnA -< t) <⊔> (throw -< ())
      else throw -< ()

  matchNum = proc (_,t) ->
    if isNumeric t
      then (returnA -< t) <⊔> (throw -< ())
      else throw -< ()

  matchExplode matchCons' matchSubterms = proc t -> case t of
    _ | isLexical t -> do
        matchSubterms -< convertToList [] (context t)
        returnA -< t
      | isNumeric t -> do
        matchSubterms -< convertToList [] (context t)
        returnA -< t
    Term (Tuple ss) ctx -> do
        matchCons' -< Term Lexical ctx
        matchSubterms -< Term (List $ foldr (Ctx.lub ctx) Bottom ss) ctx
        returnA -< t
    Term _ ctx -> do
      matchCons' -< Term Lexical ctx
      matchSubterms -< Term (List Top) ctx
      returnA -< t

  buildExplode = proc (t,ts) ->
    if isLexical t && isList ts
      then (returnA -< Term Top (context t)) <⊔> (throw -< ()) -- cannot deduct target sort from sort Lexical
      else throw -< ()

  buildCons = askConst $ \ctx -> proc (c, ss) -> case c of
    "Cons" -> case ss of
      [Term a _,Term s _] ->
        if isList (Term s ctx)
          then returnA -< Term (List a) ctx ⊔ Term s ctx
          else typeMismatch -< ("List(_)",show s)
      _ -> typeMismatch -< ("a * List(b)",show ss)
    "Nil" -> case ss of
      [] -> returnA -< Term (List Bottom) ctx
      _ -> typeMismatch -< ("List(a)",show ss)
    "" -> returnA -< Term (Tuple (map sort ss)) ctx
    _ -> let t = glb1 (Term Top ctx : [ Term s ctx | Signature ss' s <- Ctx.lookupCons ctx c, ss ⊑ sortsToTerms ss' ctx ])
         in if t == Term Bottom ctx
            then typeError -< printf "Could not construct term %s. Could not find the constructor %s in the context." (show (c,ss)) (show c)
            else returnA   -< t

  buildNum = askConst $ \ctx -> proc _ -> do
    returnA -< Term Numerical ctx

  buildString = askConst $ \ctx -> proc _ -> do
    returnA -< Term Lexical ctx

  equal = proc (t1,t2) -> case t1 ⊓ t2 of
    t | sort t == Bottom -> throw -< ()
      | isSingleton t1 && isSingleton t2 -> returnA -< t
      | otherwise -> (returnA -< t) <⊔> (throw -< ())

  mapSubterms f = askConst $ \ctx -> proc s -> do
    case sort s of
      Top ->
        typeError -< "generic traversal over top is not supported."
      s' ->
        (| joinList
          (typeError -< printf "Sort %s not found in context." (show s))
          (\(c,ts) -> do
            ts' <- f -< ts
            buildCons -< (c,ts')) |)
          ([ (c',sortsToTerms ss ctx) | (c',Signature ss _) <- Ctx.lookupSort ctx s'])

  {-# INLINE matchCons #-}
  {-# INLINE matchString #-}
  {-# INLINE matchNum #-}
  {-# INLINE matchExplode #-}
  {-# INLINE buildCons #-}
  {-# INLINE buildString #-}
  {-# INLINE buildNum #-}
  {-# INLINE buildExplode #-}
  {-# INLINE equal #-}
  {-# INLINE mapSubterms #-}

-- instance ArrowConst Context c => ArrowTop Term (EnvironmentT Term c) where
--   topA = proc () -> do
--     ctx <- askConst -< ()
--     returnA -< Term Top ctx

instance ArrowComplete Term c => ArrowComplete Term (ValueT Term c) where
  ValueT f <⊔> ValueT g = ValueT $ proc x -> f <⊔> g -< x
  {-# INLINE (<⊔>) #-}

instance Complete (FreeCompletion Term) where
  Lower x ⊔ Lower y = Lower (x ⊔ y)
  _ ⊔ _ = Free.Top

instance Show Term where
  show (Term s _) = show s

instance Eq Term where
  Term t1 _ == Term t2 _ = t1 == t2

instance Hashable Term where
  hashWithSalt salt (Term s _) = salt `hashWithSalt` s

instance NFData Term where
  rnf = rnf . sort

instance PreOrd Term where
  Term s1 ctx ⊑ Term s2 _ = Ctx.subtype ctx s1 s2

instance Complete Term where
  Term t1 ctx ⊔ Term t2 _ = Term (Ctx.lub ctx t1 t2) ctx

instance CoComplete Term where
  Term t1 ctx ⊓ Term t2 _ = Term (Ctx.glb ctx t1 t2) ctx

instance UpperBounded Term where
  top = Term Top Ctx.empty

convertToList :: [Term] -> Context -> Term
convertToList [] ctx = Term (List Bottom) ctx
convertToList ts ctx = Term (List (sort $ lub ts)) ctx

isLexical :: Term -> Bool
isLexical (Term s ctx) = Ctx.isLexical ctx s

isNumeric :: Term -> Bool
isNumeric (Term s ctx) = Ctx.isNumerical ctx s

isList :: Term -> Bool
isList (Term s ctx) = Ctx.isList ctx s

getListElem :: Term -> Term
getListElem (Term s ctx) = Term (Ctx.getListElem ctx s) ctx

isTuple :: Term -> Int -> Bool
isTuple (Term s ctx) i = Ctx.isTuple ctx i s

isSingleton :: Term -> Bool
isSingleton (Term s ctx) = Ctx.isSingleton ctx s

sortsToTerms :: [Sort] -> Context -> [Term]
sortsToTerms ss ctx = map (`Term` ctx) ss

typeMismatch :: (ArrowFail e c, IsString e) => c (String,String) a
typeMismatch = lmap (\(expected,actual) -> printf "expected type %s but got type %s" (show expected) (show actual)) typeError

typeError :: (ArrowFail e c, IsString e) => c String a
typeError = lmap fromString fail

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
