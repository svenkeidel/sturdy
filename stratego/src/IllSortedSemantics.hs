{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC
  -Wno-partial-type-signatures
  -Wno-orphans
  -fspecialise-aggressively
  -fexpose-all-unfoldings
  -funfolding-use-threshold=10000
  -flate-specialise
  -flate-dmd-anal
  -fsimpl-tick-factor=50000
  -fmax-simplifier-iterations=10
#-}
module IllSortedSemantics where

import           Prelude hiding ((.),fail)

import qualified GenericInterpreter as Generic
import           GenericInterpreter (IsTerm(..))
import           AbstractInterpreter
import qualified SortSemantics as Sort
import           Sort (Sort)
import qualified Sort as S
import           SortContext (Context,Signature(..))
import qualified SortContext as Ctx
import           Syntax (LStrat,LStratEnv,Strategy)
import           Abstract.TermEnvironment
import           Utils

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Closure
import           Control.Arrow.Trans
import           Control.Arrow.Const
import           Control.Arrow.Except
import           Control.Arrow.Fail as Fail
import           Control.DeepSeq
import           Control.Arrow.Transformer.Value
import           Control.Arrow.Order

import           Data.Abstract.Closure (Closure)
import           Data.Abstract.FreeCompletion hiding (Top)
import qualified Data.Abstract.FreeCompletion as Free
import           Data.Abstract.Except as E
import           Data.Abstract.Error as F
import           Data.Abstract.Terminating (Terminating)
import           Data.Abstract.Stable
import           Data.Abstract.Widening as W
import           Data.Hashable
import           Data.Order hiding (LowerBounded(..))
import           Data.Profunctor
import           Data.Text(Text)
import           Data.Constructor
import           Data.Abstract.Constructor(Constr)
import qualified Data.Abstract.Constructor as Constr
import           Data.Coerce
import           Data.Label

import           Text.Printf
import           Prettyprinter
import           GHC.Exts(IsList(..),IsString(..))

data Term = Sorted Sort Context | IllSorted (Constr Term) deriving (Eq)

eval :: (?sensitivity :: Int) => Int -> Int -> LStrat -> LStratEnv -> Context -> TermEnv Term -> Term -> Terminating (FreeCompletion (Error TypeError (Except () (TermEnv Term,Term))))
eval i j lstrat lsenv ctx =
  let (strat,senv) = generate $ (,) <$> lstrat <*> lsenv
  in runInterp (\algo -> let ?fixpointAlgorithm = algo in
                   Generic.eval strat) (termWidening ctx i j) senv ctx

-- Instances -----------------------------------------------------------------------------------------
instance (ArrowChoice c, ArrowApply c, ArrowComplete Term c, ArrowComplete Sort.Term c, ArrowConst Context c,
          ArrowFail e c, ArrowExcept () c, IsString e,
          ArrowLowerBounded Term c, Fail.Join Term c,
          ArrowLowerBounded Sort.Term c, Fail.Join Sort.Term c)
    => IsTerm Term (ValueT Term c) where
  matchCons matchSubterms = proc (c,ps,t) ->
    case t of
      IllSorted cs -> matchCons' -< (c,ps,toList cs)

      Sorted s ctx -> case (c,ps,s) of

        (_,_,S.Bottom) -> do
          ss' <- matchSubterms -< (ps,[t | _ <- ps])
          buildCons -< (c,ss')

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
        (| joinList (bottom -< ()) (\(c',ts) ->
            if Constructor c' == c && eqLength ps ts
            then do
              ts' <- matchSubterms -< (ps,ts)
              buildCons -< (c,ts')
            else throw -< ()) |) cs
      {-# INLINE matchCons' #-}



  matchString = proc (str,t) -> case t of
    IllSorted _ -> throw -< ()
    Sorted s ctx -> do
      Sort.Term s' _ <- liftSort matchString -< (str,Sort.Term s ctx)
      returnA -< Sorted s' ctx

  matchNum = proc (num,t) -> case t of
    IllSorted _ -> throw -< ()
    Sorted s ctx -> do
      Sort.Term s' _ <- liftSort matchNum -< (num,Sort.Term s ctx)
      returnA -< Sorted s' ctx

  matchExplode matchCons' matchSubterms = askConst $ \ctx -> proc t -> case t of
    Sorted (S.Tuple ss) _ -> do
        matchCons' -< Sorted S.Lexical ctx
        matchSubterms -< Sorted (S.List $ foldr (Ctx.lub ctx) S.Bottom ss) ctx
        returnA -< t
    Sorted s _
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
      matchCons' -< Sorted S.Lexical ctx
      matchSubterms -< convertToList (concatMap snd (toList cs)) ctx
      returnA -< t

  buildCons = askConst $ \ctx -> proc (c,ts) ->
    returnA -< illSorted [(c,[ Sorted (typecheck ctx t) ctx | t <- ts])]

  buildNum = proc n -> do
    Sort.Term s ctx <- liftSort buildNum -< n
    returnA -< Sorted s ctx

  buildString = proc str -> do
    Sort.Term s ctx <- liftSort buildString -< str
    returnA -< Sorted s ctx

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
        (| joinList (bottom -< ()) (\(c',ts) -> do
          ts' <- f -< ts
          buildCons -< (Constructor c',ts')
        ) |) l

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

liftSort :: ValueT Sort.Term c x y -> ValueT Term c x y
liftSort = coerce

deriving instance (ArrowChoice c, ArrowComplete Term c, ArrowFail e c, IsString e) => ArrowComplete Term (ValueT Term c)
deriving instance ArrowLowerBounded Term c => ArrowLowerBounded Term (ValueT Term c)
deriving instance ArrowClosure Strategy (Closure Strategy SEnv) c => ArrowClosure Strategy (Closure Strategy SEnv) (ValueT Term c)
instance ArrowTrans (ValueT Term) where
  lift' = ValueT
  {-# INLINE lift' #-}


instance Complete (FreeCompletion Term) where
  Lower x ⊔ Lower y = Lower (x ⊔ y)
  _ ⊔ _ = Free.Top

instance PreOrd Term where
  Sorted S.Bottom _ ⊑ _ = True
  _ ⊑ Sorted S.Top _ = True
  Sorted s1 ctx ⊑ Sorted s2 _  = Ctx.subtype ctx s1 s2
  IllSorted cs1 ⊑ IllSorted cs2 = cs1 ⊑ cs2
  IllSorted cs ⊑ Sorted s ctx = Ctx.subtype ctx (typecheck ctx (IllSorted cs)) s
  Sorted S.Top _ ⊑ IllSorted _ = False
  Sorted s ctx ⊑ IllSorted cs = IllSorted (fromList (lookupSort ctx s)) ⊑ IllSorted cs

instance Complete Term where
  Sorted S.Top ctx ⊔ _ = Sorted S.Top ctx
  _ ⊔ Sorted S.Top ctx = Sorted S.Top ctx
  Sorted S.Bottom _ ⊔ t = t
  t ⊔ Sorted S.Bottom _ = t
  Sorted s1 ctx ⊔ Sorted s2 _ = Sorted (Ctx.lub ctx s1 s2) ctx
  IllSorted cs1 ⊔ IllSorted cs2 = IllSorted (cs1 ⊔ cs2)
  Sorted s1 ctx ⊔ IllSorted cs2 = IllSorted (fromList (lookupSort ctx s1) ⊔ cs2)
  IllSorted cs1 ⊔ Sorted s2 ctx = Sorted s2 ctx ⊔ IllSorted cs1

instance CoComplete Term where
  Sorted S.Top _ ⊓ t = t
  t ⊓ Sorted S.Top _ = t
  Sorted S.Bottom ctx ⊓ _ = Sorted S.Bottom ctx
  _ ⊓ Sorted S.Bottom ctx = Sorted S.Bottom ctx
  Sorted s1 ctx ⊓ Sorted s2 _ = Sorted (Ctx.glb ctx s1 s2) ctx
  IllSorted cs1 ⊓ IllSorted cs2 = IllSorted (cs1 ⊓ cs2)
  Sorted s1 ctx ⊓ IllSorted cs2 = IllSorted (fromList (lookupSort ctx s1) ⊓ cs2)
  IllSorted cs1 ⊓ Sorted s2 ctx = Sorted s2 ctx ⊓ IllSorted cs1

instance UpperBounded Term where
  top = Sorted S.Top Ctx.empty

instance Show Term where
  show (Sorted s _) = show s
  show (IllSorted c) = show c

instance Pretty Term where
  pretty = viaShow

instance Hashable Term where
  hashWithSalt s (Sorted so _) = s `hashWithSalt` (1 :: Int) `hashWithSalt` so
  hashWithSalt s (IllSorted cs) = s `hashWithSalt` (2 :: Int) `hashWithSalt` cs

instance NFData Term where
  rnf (Sorted s _) = rnf s
  rnf (IllSorted cs) = rnf cs

instance IsList Term where
  type Item Term = Item (Constr Term)
  fromList l = IllSorted (fromList l)
  toList (IllSorted l) = toList l
  toList (Sorted s ctx) = lookupSort ctx s

instance IsString Term where
  fromString s = Sorted (fromString s) Ctx.empty

termWidening :: Context -> Int -> Int -> Widening Term
termWidening ctx _ j (Sorted s1 _) (Sorted s2 _) = (`Sorted` ctx) <$> Ctx.widening ctx j s1 s2
termWidening ctx k j (IllSorted cs1) (IllSorted cs2)
  | k == 0    = let s = Sorted (typecheck ctx (IllSorted (cs1 ⊔ cs2))) ctx in (if s ⊑ IllSorted (cs1 ⊔ cs2) then Stable else Unstable, s)
  | otherwise = IllSorted <$> Constr.widening (termWidening ctx (k-1) j) cs1 cs2
termWidening ctx _ _ (Sorted S.Top _) (IllSorted _) = (Stable,Sorted S.Top ctx)
termWidening ctx k j (Sorted s1 _) (IllSorted cs2) = termWidening ctx k j (IllSorted (fromList (lookupSort ctx s1))) (IllSorted cs2)
termWidening ctx _ _ (IllSorted _) (Sorted S.Top _) = (Stable,Sorted S.Top ctx)
termWidening ctx k j (IllSorted cs1) (Sorted s2 _) = termWidening ctx k j (IllSorted cs1) (IllSorted (fromList (lookupSort ctx s2)))

typecheck :: Context -> Term -> Sort
typecheck ctx t = case t of
  Sorted s _ -> s
  IllSorted cs -> foldr1 (Ctx.lub ctx) [ checkCons (Constructor c) (typecheck ctx <$> ts) | (c,ts) <- toList cs]
  where
    checkCons :: Constructor -> [Sort] -> Sort
    checkCons c ss =
      case (c,ss) of
        ("Cons",[a,s])
          | Ctx.isList ctx s -> Ctx.lub ctx (S.List a) s
          | otherwise -> S.Top
        ("Nil",[]) -> S.List S.Bottom
        ("",[S.Lexical]) -> S.Lexical
        ("",[S.Numerical]) -> S.Numerical
        ("",_) -> S.Tuple ss
        _ -> getSort $ glb (Sorted S.Top ctx) [ Sorted s ctx | Signature ss' s <- Ctx.lookupCons ctx c, map (`Sorted` ctx) ss ⊑ sortsToTerms ss' ctx ]

typecheck' :: Context -> Term -> Term
typecheck' ctx t = Sorted (typecheck ctx t) ctx

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
lookupSort ctx s = (coerce *** sigToTerm ctx) <$> Ctx.lookupSort ctx s

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

typeMismatch :: (ArrowFail e c, Fail.Join a c, IsString e) => c (String,String) a
typeMismatch = lmap (\(expected,actual) -> printf "expected type %s but got type %s" (show expected) (show actual)) typeError

typeError :: (ArrowFail e c, Fail.Join a c, IsString e) => c String a
typeError = lmap fromString fail
