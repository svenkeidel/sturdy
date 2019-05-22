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
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module ConstructorSemantics where

import           Prelude hiding ((.),fail)

import qualified ConcreteSemantics as C
import           SharedSemantics as Shared
import           AbstractSemantics
import           Sort (Sort)
import qualified Sort as S
import           SortContext (Context,Signature(..))
import qualified SortContext as Ctx
import           Syntax hiding (Fail,TermPattern(..))
import           Utils
import           ValueT

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Const
import           Control.Arrow.Except
import           Control.Arrow.Fail
import           Control.Arrow.Abstract.Join
import           Control.DeepSeq

import           Data.Abstract.FreeCompletion hiding (Top)
import qualified Data.Abstract.FreeCompletion as Free
import           Data.Abstract.Except as E
import           Data.Abstract.Error as F
import           Data.Abstract.Terminating (Terminating)
import           Data.Abstract.Widening as W
import qualified Data.Abstract.TreeGrammar.Terminal as Term
import           Data.Hashable
import           Data.Order
import           Data.Profunctor
import           Data.Text(Text)
import           Data.Identifiable
import           Data.Constructor
import           Data.Coerce
import           Data.Maybe(isJust)
import           Data.TermEnvironment

import           Text.Printf
import           GHC.Exts(IsList(..),IsString(..))
import           GHC.Generics(Generic)

data Term = Sort Sort Context | Constr (Term.Constr Term) deriving (Eq,Show,Generic)


eval :: Int -> Int -> Strat -> StratEnv -> Context -> TermEnv Term -> Term -> Terminating (FreeCompletion (Error TypeError (Except () (TermEnv Term,Term))))
eval i j strat senv ctx  = runInterp (Shared.eval' strat) i _ senv ctx

-- Instances -----------------------------------------------------------------------------------------
instance (ArrowChoice c, ArrowApply c, ArrowJoin c, ArrowConst Context c, ArrowFail e c, ArrowExcept () c,
          IsString e, LowerBounded (c () Term))
    => IsTerm Term (ValueT Term c) where
  matchCons matchSubterms = proc (c,ps,t) ->
    case t of
      Constr cs ->
        (| joinList (typeError -< printf "encountered empty set of constructors while matching %s" (show c)) (\(c',ts) ->
            if Constructor c' == c && eqLength ps ts
            then do
              ts' <- matchSubterms -< (ps,ts)
              buildCons -< (c,ts')
            else throw -< ()) |) (toList cs)

      Sort s ctx -> case (c,ps,s) of

        (_,_,S.Bottom) -> bottom -< ()

        ("Cons",[_,_],_) | Ctx.isList ctx s ->
          do matchCons matchSubterms -< (c,ps,constr [("Cons",[Sort (Ctx.getListElem ctx s) ctx, t])])
          <⊔>
          do throw -< ()
        ("Cons",_,_) -> typeMismatch -< ("List",show s)
        ("Nil",[],_) | Ctx.isList ctx s ->
          do matchCons matchSubterms -< (c,ps,constr [("Nil",[])])
          <⊔>
          do throw -< ()
        ("Nil",_,_) -> throw -< ()

        ("",_,S.Tuple ss) ->
          matchCons matchSubterms -< (c,ps,constr [("",sortsToTerms ss ctx)])
        ("",_,S.Top) ->
          do matchCons matchSubterms -< (c,ps,constr [("",[Sort S.Top ctx | _ <- ps ])])
          <⊔>
          do throw -< ()

        (_,_,S.Top) ->
          do matchCons matchSubterms -< (c,ps,lookupCons ctx c)
          <⊔>
          do throw -< ()

        _ -> matchCons matchSubterms -< (c,ps,lookupSort ctx s)


  matchString = proc (s,t) -> case t of
    Constr _ -> throw -< ()
    Sort s ctx -> if Ctx.isLexical ctx s
      then (returnA -< Sort s ctx) <⊔> (throw -< ())
      else throw -< ()

  matchNum = proc (s,t) -> case t of
    Constr _ -> throw -< ()
    Sort s ctx -> if Ctx.isNumerical ctx s
      then (returnA -< t) <⊔> (throw -< ())
      else throw -< ()

  matchExplode _ _ = _

  buildCons = proc (c,ts) -> case allSorts ts of
    Nothing -> returnA -< constr [(c,ts)]
    Just ss -> do
      ctx <- askConst -< ()
      case c of
        "Cons" -> case ss of
          [a,s] ->
            if Ctx.isList ctx s
              then returnA -< Sort (S.List a) ctx ⊔ Sort s ctx
              else typeMismatch -< ("List(_)",show s)
          _ -> typeMismatch -< ("a * List(b)",show ss)
        "Nil" -> case ss of
          [] -> returnA -< Sort (S.List S.Bottom) ctx
          _ -> typeMismatch -< ("List(a)",show ss)
        "" -> returnA -< Sort (S.Tuple ss) ctx
        _ -> let t = glb (Sort S.Top ctx : [ Sort s ctx | Signature ss' s <- Ctx.lookupCons ctx c, map (`Sort` ctx) ss ⊑ sortsToTerms ss' ctx ])
             in if t == Sort S.Bottom ctx
                then typeError -< printf "Could not construct term %s. Could not find the constructor %s in the context." (show (c,ss)) (show c)
                else returnA   -< t

  buildNum = proc _ -> do
    ctx <- askConst -< ()
    returnA -< Sort S.Numerical ctx

  buildString = proc _ -> do
    ctx <- askConst -< ()
    returnA -< Sort S.Lexical ctx

  buildExplode = _

  equal = proc (t1,t2) -> case t1 ⊓ t2 of
    t | isBottom t -> throw -< ()
      | isSingleton t1 && isSingleton t2 -> returnA -< t
      | otherwise -> (returnA -< t) <⊔> (throw -< ())

  mapSubterms f = proc t -> case t of
    Constr cs ->
      (| joinList (typeError -< printf "encountered empty set of constructors") (\(c',ts) -> do
          ts' <- f -< ts
          buildCons -< (Constructor c',ts')
        ) |) (toList cs)
    Sort s ctx -> mapSubterms f -< lookupSort ctx s

instance ArrowConst Context c => ArrowTop Term (EnvironmentT Term c) where
  topA = proc () -> do
    ctx <- askConst -< ()
    returnA -< Sort S.Top ctx

instance Complete (FreeCompletion Term) where
  Lower x ⊔ Lower y = Lower (x ⊔ y)
  _ ⊔ _ = Free.Top

instance PreOrd Term where
  Sort s1 ctx ⊑ Sort s2 _  = Ctx.subtype ctx s1 s2
  Constr cs1  ⊑ Constr cs2 = isJust $ Term.subsetOf _ cs1 cs2

instance Complete Term where
  _ ⊔ _ = _

instance CoComplete Term where
  _ ⊓ _ = _

instance Hashable Term
instance NFData (Term)

isBottom :: Term -> Bool
isBottom (Sort S.Bottom _) = True
isBottom (Constr cs) = False

allSorts :: [Term] -> Maybe [Sort]
allSorts = mapM (\(Sort s _) -> Just s)

constr :: [(Constructor,[Term])] -> Term
constr = Constr . fromList . coerce

lookupCons :: Context -> Constructor -> Term
lookupCons ctx c = constr $ (const c &&& sigToTerm ctx) <$> Ctx.lookupCons ctx c

lookupSort :: Context -> Sort -> Term
lookupSort ctx s = constr $ second (sigToTerm ctx) <$> Ctx.lookupSort ctx s

sigToTerm :: Context -> Signature -> [Term]
sigToTerm ctx (Signature ss _) = map (`Sort` ctx) ss

isSingleton :: Term -> Bool
isSingleton (Sort s ctx) = Ctx.isSingleton ctx s
isSingleton (Constr cs) = _

sortsToTerms :: [Sort] -> Context -> [Term]
sortsToTerms ss ctx = map (`Sort` ctx) ss

typeMismatch :: (ArrowFail e c, IsString e) => c (String,String) a
typeMismatch = lmap (\(expected,actual) -> printf "expected type %s but got type %s" (show expected) (show actual)) typeError

typeError :: (ArrowFail e c, IsString e) => c String a
typeError = lmap fromString fail
