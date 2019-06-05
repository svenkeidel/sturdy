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
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module IllSortedSemantics where

import           Prelude hiding ((.),fail)

import qualified SharedSemantics as Shared
import           SharedSemantics (IsTerm(..))
import           AbstractSemantics
import           Sort (Sort)
import qualified Sort as S
import           SortContext (Context,Signature(..))
import qualified SortContext as Ctx
import           Syntax hiding (Fail,TermPattern(..))
import           Utils
import           Data.TermEnvironment

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
import           Data.Hashable
import           Data.Order
import           Data.Profunctor
import           Data.Text(Text)
import           Data.Constructor
import           Data.Abstract.Constructor(Constr)
import qualified Data.Abstract.Constructor as Constr
import           Data.Coerce

import           Text.Printf
import           GHC.Exts(IsList(..),IsString(..))

data Term = Sorted Sort Context | IllSorted (Constr Term) deriving (Eq)

eval :: Int -> Int -> Strat -> StratEnv -> Context -> TermEnv Term -> Term -> Terminating (FreeCompletion (Error TypeError (Except () (TermEnv Term,Term))))
eval i j strat senv ctx  = runInterp (Shared.eval' strat) i (termWidening ctx j) senv ctx

-- Instances -----------------------------------------------------------------------------------------
instance (ArrowChoice c, ArrowApply c, ArrowJoin c, ArrowConst Context c, ArrowFail e c, ArrowExcept () c,
          IsString e, LowerBounded (c () Term))
    => IsTerm Term (ValueT Term c) where
  matchCons matchSubterms = proc (c,ps,t) ->
    case t of
      IllSorted cs -> matchCons' -< (c,ps,toList cs)

      Sorted s ctx -> case (c,ps,s) of

        (_,_,S.Bottom) -> bottom -< ()

        ("Cons",[_,_],_) | Ctx.isList ctx s ->
          matchCons' -< (c,ps,[("Cons",[Sorted (Ctx.getListElem ctx s) ctx, t]), ("Nil",[])])
        ("Cons",_,_) -> typeMismatch -< ("List",show s)
        ("Nil",[],_) | Ctx.isList ctx s ->
          matchCons' -< (c,ps,[("Cons",[Sorted (Ctx.getListElem ctx s) ctx, t]),("Nil",[])])
        ("Nil",_,_) -> throw -< ()

        ("",_,S.Tuple ss) ->
          matchCons' -< (c,ps,[("",sortsToTerms ss ctx)])
        ("",_,S.Top) ->
          do matchCons' -< (c,ps,[("",[Sorted S.Top ctx | _ <- ps ])])
          <⊔>
          do throw -< ()

        (_,_,S.Top) ->
          do matchCons' -< (c,ps,lookupCons ctx c)
          <⊔>
          do throw -< ()

        _ -> matchCons' -< (c,ps,lookupSort ctx s)

    where
      matchCons' = proc (c,ps,cs) ->
        (| joinList (typeError -< printf "encountered empty set of constructors while matching %s" (show c)) (\(c',ts) ->
            if Constructor c' == c && eqLength ps ts
            then do
              ts' <- matchSubterms -< (ps,ts)
              buildCons -< (c,ts')
            else throw -< ()) |) cs



  matchString = proc (_,t) -> case t of
    IllSorted _ -> throw -< ()
    Sorted s ctx -> if Ctx.isLexical ctx s
      then (returnA -< Sorted s ctx) <⊔> (throw -< ())
      else throw -< ()

  matchNum = proc (_,t) -> case t of
    IllSorted _ -> throw -< ()
    Sorted s ctx -> if Ctx.isNumerical ctx s
      then (returnA -< t) <⊔> (throw -< ())
      else throw -< ()

  matchExplode matchCons' matchSubterms = proc t -> case t of
    Sorted (S.Tuple ss) ctx -> do
        matchCons' -< Sorted S.Lexical ctx
        matchSubterms -< Sorted (S.List $ foldr (Ctx.lub ctx) S.Bottom ss) ctx
        returnA -< t
    Sorted s ctx
      | Ctx.isLexical ctx s -> do
        matchSubterms -< convertToList [] ctx
        returnA -< t
      | Ctx.isNumerical ctx s -> do
        matchSubterms -< convertToList [] ctx
        returnA -< t
      | otherwise -> do
        matchCons' -< Sorted S.Lexical ctx
        matchSubterms -< Sorted (S.List S.Top) ctx
        returnA -< t
    IllSorted cs -> do
      ctx <- askConst -< ()
      matchCons' -< Sorted S.Lexical ctx
      matchSubterms -< convertToList (concatMap snd (toList cs)) ctx
      returnA -< t


  buildCons = proc (c,ts) ->
    returnA -< illSorted [(c,ts)]
    -- case allSorted ts of
    -- Nothing -> returnA -< illSorted [(c,ts)]
    -- Just ss -> do
    --   ctx <- askConst -< ()
    --   case c of
    --     "Cons" -> case ss of
    --       [a,s] ->
    --         if Ctx.isList ctx s
    --           then returnA -< Sorted (S.List a) ctx ⊔ Sorted s ctx
    --           else typeMismatch -< ("List(_)",show s)
    --       _ -> typeMismatch -< ("a * List(b)",show ss)
    --     "Nil" -> case ss of
    --       [] -> returnA -< Sorted (S.List S.Bottom) ctx
    --       _ -> typeMismatch -< ("List(a)",show ss)
    --     "" -> returnA -< Sorted (S.Tuple ss) ctx
    --     _ -> let t = glb (Sorted S.Bottom ctx) [ Sorted s ctx | Signature ss' s <- Ctx.lookupCons ctx c, map (`Sorted` ctx) ss ⊑ sortsToTerms ss' ctx ]
    --          in if t == Sorted S.Bottom ctx
    --             -- In case suitable signature could be found in the context, return an ill-sorted term instead.
    --             then returnA -< illSorted [(c,ts)]
    --             else returnA -< t

  buildNum = proc _ -> do
    ctx <- askConst -< ()
    returnA -< Sorted S.Numerical ctx

  buildString = proc _ -> do
    ctx <- askConst -< ()
    returnA -< Sorted S.Lexical ctx

  buildExplode = proc (t,ts) -> case t of
    Sorted s1 ctx
      | Ctx.isLexical ctx s1 && isList ts -> returnA -< Sorted S.Top ctx
      where
        isList :: Term -> Bool
        isList (Sorted s _) = Ctx.isList ctx s
        isList (IllSorted cs) = all (\case ("Cons",[_,r]) -> isList r; ("Nil",[]) -> True; _ -> False) (toList cs)

    _ -> throw -< ()


  equal = proc (t1,t2) -> case t1 ⊓ t2 of
    t | isBottom t -> throw -< ()
      | isSingleton t1 && isSingleton t2 -> returnA -< t
      | otherwise -> (returnA -< t) <⊔> (throw -< ())

  mapSubterms f = proc t -> case t of
    IllSorted cs -> mapSubterms' -< toList cs
    Sorted S.Top _ -> typeError -< "generic traversal over top is not supported."
    Sorted s ctx -> mapSubterms' -< lookupSort ctx s
    where
      mapSubterms' = proc l ->
        (| joinList (typeError -< printf "encountered empty set of constructors") (\(c',ts) -> do
          ts' <- f -< ts
          buildCons -< (Constructor c',ts')
        ) |) l

instance ArrowConst Context c => ArrowTop Term (EnvironmentT Term c) where
  topA = proc () -> do
    ctx <- askConst -< ()
    returnA -< Sorted S.Top ctx

instance Complete (FreeCompletion Term) where
  Lower x ⊔ Lower y = Lower (x ⊔ y)
  _ ⊔ _ = Free.Top

instance PreOrd Term where
  Sorted S.Bottom _ ⊑ _ = True
  _ ⊑ Sorted S.Top _ = True
  Sorted s1 ctx ⊑ Sorted s2 _  = Ctx.subtype ctx s1 s2
  IllSorted cs1 ⊑ IllSorted cs2 = cs1 ⊑ cs2
  IllSorted cs ⊑ Sorted _ _ = Constr.isEmpty cs
  Sorted s ctx ⊑ IllSorted cs
    | Ctx.isLexical ctx s || Ctx.isNumerical ctx s = False
    | otherwise = IllSorted (fromList (lookupSort ctx s)) ⊑ IllSorted cs

instance Complete Term where
  Sorted S.Top ctx ⊔ _ = Sorted S.Top ctx
  _ ⊔ Sorted S.Top ctx = Sorted S.Top ctx
  Sorted S.Bottom _ ⊔ t = t
  t ⊔ Sorted S.Bottom _ = t
  Sorted s1 ctx ⊔ Sorted s2 _ = Sorted (Ctx.lub ctx s1 s2) ctx
  IllSorted cs1 ⊔ IllSorted cs2 = IllSorted (cs1 ⊔ cs2)
  Sorted s1 ctx ⊔ IllSorted cs2
    | Ctx.isLexical ctx s1 || Ctx.isNumerical ctx s1 = Sorted S.Top ctx
    | otherwise = IllSorted (fromList (lookupSort ctx s1) ⊔ cs2)
  IllSorted cs1 ⊔ Sorted s2 ctx = Sorted s2 ctx ⊔ IllSorted cs1

instance CoComplete Term where
  Sorted S.Top _ ⊓ t = t
  t ⊓ Sorted S.Top _ = t
  Sorted S.Bottom ctx ⊓ _ = Sorted S.Bottom ctx
  _ ⊓ Sorted S.Bottom ctx = Sorted S.Bottom ctx
  Sorted s1 ctx ⊓ Sorted s2 _ = Sorted (Ctx.glb ctx s1 s2) ctx
  IllSorted cs1 ⊓ IllSorted cs2 = IllSorted (cs1 ⊓ cs2)
  Sorted s1 ctx ⊓ IllSorted cs2
    | Ctx.isLexical ctx s1 || Ctx.isNumerical ctx s1 = Sorted S.Bottom ctx
    | otherwise = IllSorted (fromList (lookupSort ctx s1) ⊓ cs2)
  IllSorted cs1 ⊓ Sorted s2 ctx = Sorted s2 ctx ⊓ IllSorted cs1

instance Show Term where
  show (Sorted s _) = show s
  show (IllSorted c) = show c

instance Hashable Term where
  hashWithSalt s (Sorted so _) = s `hashWithSalt` (1 :: Int) `hashWithSalt` so
  hashWithSalt s (IllSorted cs) = s `hashWithSalt` (2 :: Int) `hashWithSalt` cs

instance NFData Term where
  rnf (Sorted s _) = rnf s
  rnf (IllSorted cs) = rnf cs

-- termWidening ctx k t1 t2 = let t3 = go ctx k t1 t2
--                            in trace (printf "%s ▽ %s = %s" (show t1) (show t2) (show t3)) () `seq` t3

termWidening :: Context -> Int -> Widening Term
termWidening ctx k (Sorted s1 _) (Sorted s2 _) = (`Sorted` ctx) <$> S.widening k s1 s2
termWidening ctx k (IllSorted cs1) (IllSorted cs2)
  | k == 0    = let s = Sorted (typecheck ctx (IllSorted (cs1 ⊔ cs2))) ctx in (if s ⊑ IllSorted (cs1 ⊔ cs2) then Stable else Instable, s)
  | otherwise = IllSorted <$> Constr.widening (termWidening ctx (k-1)) cs1 cs2
termWidening ctx k (Sorted s1 _) (IllSorted cs2) = termWidening ctx k (IllSorted (fromList (lookupSort ctx s1))) (IllSorted cs2)
termWidening ctx k (IllSorted cs1) (Sorted s2 _) = termWidening ctx k (IllSorted cs1) (IllSorted (fromList (lookupSort ctx s2)))

typecheck :: Context -> Term -> Sort
typecheck ctx t = case t of
  Sorted s _ -> s
  IllSorted cs -> foldr1 (Ctx.lub ctx) [ checkCons (Constructor c) (typecheck ctx <$> ts) | (c,ts) <- toList cs]
  where
    checkCons :: Constructor -> [Sort] -> Sort
    checkCons c ss =
      case c of
        "Cons" -> case ss of
          [a,s] | Ctx.isList ctx s -> Ctx.lub ctx (S.List a) s
          _ -> S.Top
        "Nil" -> case ss of
          [] -> S.List S.Bottom
          _ -> S.Top
        "" -> S.Tuple ss
        _ -> getSort $ glb (Sorted S.Top ctx) [ Sorted s ctx | Signature ss' s <- Ctx.lookupCons ctx c, map (`Sorted` ctx) ss ⊑ sortsToTerms ss' ctx ]

getSort :: Term -> Sort
getSort (Sorted s _) = s
getSort _ = error "cannot get sort of ill-sorted term"

isBottom :: Term -> Bool
isBottom (Sorted S.Bottom _) = True
isBottom (IllSorted cs) = Constr.isEmpty cs
isBottom _ = False

allSorted :: [Term] -> Maybe [Sort]
allSorted = mapM (\case (Sorted s _) -> Just s; _ -> Nothing)

illSorted :: [(Constructor,[Term])] -> Term
illSorted = IllSorted . fromList . coerce

lookupCons :: Context -> Constructor -> [(Text,[Term])]
lookupCons ctx c = (const (coerce c) &&& sigToTerm ctx) <$> Ctx.lookupCons ctx c

lookupSort :: Context -> Sort -> [(Text,[Term])]
lookupSort ctx s = (coerce *** (sigToTerm ctx)) <$> Ctx.lookupSort ctx s

sigToTerm :: Context -> Signature -> [Term]
sigToTerm ctx (Signature ss _) = map (`Sorted` ctx) ss

isSingleton :: Term -> Bool
isSingleton (Sorted s ctx) = Ctx.isSingleton ctx s
isSingleton (IllSorted cs) = Constr.isSingleton isSingleton cs

sortsToTerms :: [Sort] -> Context -> [Term]
sortsToTerms ss ctx = map (`Sorted` ctx) ss

convertToList :: [Term] -> Context -> Term
convertToList [] ctx = Sorted (S.List S.Bottom) ctx
convertToList ts ctx = Sorted (S.List (toSort $ lub ts)) ctx
  where
    toSort :: Term -> Sort
    toSort (Sorted s _) = s
    toSort (IllSorted _) = S.Top

typeMismatch :: (ArrowFail e c, IsString e) => c (String,String) a
typeMismatch = lmap (\(expected,actual) -> printf "expected type %s but got type %s" (show expected) (show actual)) typeError

typeError :: (ArrowFail e c, IsString e) => c String a
typeError = lmap fromString fail
