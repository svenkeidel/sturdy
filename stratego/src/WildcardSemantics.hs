{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures -fno-warn-orphans #-}
module WildcardSemantics where

import           Prelude hiding ((.),fail,Just,Nothing)

import qualified ConcreteSemantics as C
import           SharedSemantics
import           Soundness
import           Syntax hiding (Fail,TermPattern(..))
import           Utils

import           Control.Arrow
import           Control.Arrow.Deduplicate
import           Control.Arrow.Except
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.Reader
import           Control.Arrow.State
import           Control.Arrow.Transformer.Abstract.Fixpoint
import           Control.Arrow.Transformer.Abstract.HandleExcept
import           Control.Arrow.Transformer.Abstract.Powerset
import           Control.Arrow.Transformer.Reader
import           Control.Arrow.Transformer.State
import           Control.Category
import           Control.DeepSeq
import           Control.Monad hiding (fail)

import           Data.Abstract.FreeCompletion
import           Data.Abstract.HandleError
import           Data.Abstract.Maybe
import qualified Data.Abstract.Powerset as A
import           Data.Abstract.PreciseStore (Store)
import qualified Data.Abstract.PreciseStore as S
import qualified Data.Abstract.StackWidening as SW
import           Data.Abstract.Terminating (Terminating,fromTerminating)
import           Data.Abstract.Widening as W
import qualified Data.Concrete.Powerset as CP
import           Data.Constructor
import           Data.Foldable (foldr')
import           Data.GaloisConnection
import qualified Data.HashMap.Lazy as M
import           Data.Hashable
import           Data.Monoidal
import           Data.Order
import           Data.Term
import           Data.Text (Text)

import           Test.QuickCheck hiding (Success)
import           Text.Printf

-- | Abstract terms in this semantics contain wildcards that represent
-- every possible concrete term.
data Term
    = Cons Constructor [Term]
    | StringLiteral Text
    | NumberLiteral Int
    | Wildcard
    deriving (Eq)

type TermEnv = Store TermVar Term

-- | 
newtype Interp s a b =
  Interp (
    Fix (Strat,Term) Term
      (Reader StratEnv
        (State TermEnv
          (Except ()
            (Powerset
              (Fixpoint s () ()
                (->)))))) a b)

runInterp :: Interp (SW.Categories (Strat,StratEnv) (TermEnv, Term) SW.Stack) a b -> Int -> StratEnv -> TermEnv -> a -> Terminating (A.Pow (Error () (TermEnv,b)))
runInterp (Interp f) k senv tenv a =
  runFix' stackWidening W.finite
    (runPowerset
      (runExcept
        (runState
          (runReader f))))
    (tenv, (senv, a))
  where
    stackWidening :: SW.StackWidening (SW.Categories (Strat,StratEnv) (TermEnv, Term) SW.Stack) (TermEnv, (StratEnv, (Strat, Term)))
    stackWidening = SW.categorize (Iso from' to') (SW.stack (SW.maxSize k SW.topOut))

from' :: (TermEnv, (StratEnv, (Strat, Term))) -> ((Strat, StratEnv), (TermEnv, Term))
from' (tenv,(senv,(s,t))) = ((s,senv),(tenv,t))

to' :: ((Strat, StratEnv), (TermEnv, Term)) -> (TermEnv, (StratEnv, (Strat, Term)))
to' ((s,senv),(tenv,t)) = (tenv,(senv,(s,t)))

eval :: Int -> Strat -> StratEnv -> TermEnv -> Term -> Terminating (A.Pow (Error () (TermEnv,Term)))
eval i s = runInterp (eval' s) i

-- Instances -----------------------------------------------------------------------------------------
deriving instance Category (Interp s)
deriving instance Arrow (Interp s)
deriving instance ArrowChoice (Interp s)
deriving instance ArrowReader StratEnv (Interp s)
deriving instance ArrowState TermEnv (Interp s)
deriving instance ArrowFail () (Interp s)
deriving instance ArrowFix (Strat,Term) Term (Interp s)
deriving instance PreOrd y => ArrowExcept x y () (Interp s)
deriving instance ArrowDeduplicate Term Term (Interp s)
deriving instance PreOrd b => PreOrd (Interp s a b)
deriving instance (Complete  b, PreOrd b) => Complete (Interp s a b)

instance ArrowApply (Interp s) where
  app = Interp $ (\(Interp f, b) -> (f,b)) ^>> app

instance HasStratEnv (Interp s) where
  readStratEnv = Interp (const () ^>> ask)
  localStratEnv senv f = proc a -> do
    r <- local f -< (senv,a)
    returnA -< r

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

instance IsTerm Term (Interp s) where
  matchTermAgainstConstructor matchSubterms = proc (c,ts,t) -> case t of
    Cons c' ts' | c == c' && eqLength ts ts' -> do
      ts'' <- matchSubterms -< (ts,ts')
      returnA -< Cons c ts''
    Wildcard -> do
      ts'' <- matchSubterms -< (ts,[ Wildcard | _ <- [1..(length ts)] ])
      returnA ⊔ fail' -< Cons c ts''
    _ -> fail -< ()
  
  matchTermAgainstString = proc (s,t) -> case t of
    StringLiteral s'
      | s == s' -> returnA -< t
      | otherwise -> fail -< ()
    Wildcard ->
      returnA ⊔ fail' -< StringLiteral s
    _ -> fail' -< ()
  
  matchTermAgainstNumber = proc (n,t) -> case t of
    NumberLiteral n'
      | n == n' -> returnA -< t
      | otherwise -> fail -< ()
    Wildcard ->
      success ⊔ fail' -< NumberLiteral n
    _ -> fail' -< ()
  
  matchTermAgainstExplode matchCons matchSubterms = proc t -> case t of
      Cons (Constructor c) ts -> do
        matchCons -< StringLiteral c
        matchSubterms -< convertToList ts
        returnA -< t
      StringLiteral _ -> do
        matchSubterms -< convertToList []
        returnA -< t
      NumberLiteral _ -> do
        matchSubterms -< convertToList []
        returnA -< t
      Wildcard ->
        (proc t -> do
           matchCons -< Wildcard
           matchSubterms -< Wildcard
           returnA -< t)
        ⊔
        (proc t -> do
           matchSubterms -< convertToList []
           returnA -< t)
        -< t
  
  equal = proc (t1,t2) ->
    case (t1,t2) of
      (Cons c ts, Cons c' ts')
          | c == c' && eqLength ts ts' -> do
          ts'' <- zipWithA equal -< (ts,ts')
          returnA -< Cons c ts''
          | otherwise -> fail -< ()
      (StringLiteral s, StringLiteral s')
          | s == s' -> success -< t1
          | otherwise -> fail -< ()
      (NumberLiteral n, NumberLiteral n')
          | n == n' -> success -< t1
          | otherwise -> fail -< ()
      (Wildcard, t) -> fail' ⊔ success -< t
      (t, Wildcard) -> fail' ⊔ success -< t
      (_,_) -> fail' -< ()
  
  convertFromList = proc (c,ts) -> case (c,go ts) of
    (StringLiteral c', Just ts'') -> returnA -< Cons (Constructor c') ts''
    (Wildcard, Just _)            -> fail' ⊔ success -< Wildcard
    (_,                Nothing)   -> fail' ⊔ success -< Wildcard
    _                             -> fail' -< ()
    where
      go t = case t of
        Cons "Cons" [x,tl] -> (x:) <$> go tl
        Cons "Nil" [] -> Just []
        Wildcard -> Nothing 
        _ -> Nothing
  
  mapSubterms f = proc t ->
    case t of
      Cons c ts -> do
        ts' <- f -< ts
        returnA -< Cons c ts'
      StringLiteral _ -> returnA -< t
      NumberLiteral _ -> returnA -< t
      Wildcard -> fail' ⊔ success -< Wildcard
  
  cons = arr (uncurry Cons)
  numberLiteral = arr NumberLiteral
  stringLiteral = arr StringLiteral

instance Soundness StratEnv (Interp (SW.Categories (Strat,StratEnv) (TermEnv, Term) SW.Stack)) where
  sound senv xs f g = forAll (choose (0,3)) $ \i ->
    let con :: A.Pow (Error () (TermEnv,_))
        con = A.dedup $ alpha (fmap (\(x,tenv) -> C.runInterp f senv tenv x) xs)
        abst :: A.Pow (Error () (TermEnv,_))
        abst = A.dedup $ fromTerminating (error "non-terminating wildcard semantics") $ runInterp g i senv (alpha (fmap snd xs)) (alpha (fmap fst xs))
    in counterexample (printf "%s ⊑/ %s" (show con) (show abst)) $ con ⊑ abst

instance UpperBounded Term where
  top = Wildcard

instance TermUtils Term where
  convertToList ts = case ts of
    (x:xs) -> Cons "Cons" [x,convertToList xs]
    []     -> Cons "Nil" []
    
  size (Cons _ ts) = sum (size <$> ts) + 1
  size (StringLiteral _) = 1
  size (NumberLiteral _) = 1
  size Wildcard = 1

  height (Cons _ []) = 1
  height (Cons _ ts) = maximum (height <$> ts) + 1
  height (StringLiteral _) = 1
  height (NumberLiteral _) = 1
  height Wildcard = 1

instance PreOrd Term where
  t1 ⊑ t2 = case (t1,t2) of
    (_,Wildcard) -> True
    (Cons c ts,Cons c' ts') -> c == c' && (ts ⊑ ts')
    (StringLiteral s, StringLiteral s') -> s == s'
    (NumberLiteral n, NumberLiteral n') -> n == n'
    (_, _) -> False

instance PreOrd a => Complete (FreeCompletion a) where
  Lower a ⊔ Lower b
    | a ⊑ b = Lower b
    | b ⊑ a = Lower a
    | otherwise = Top
  Top ⊔ _ = Top
  _ ⊔ Top = Top

instance Complete Term where
  t1 ⊔ t2 = case (t1,t2) of
    (Cons c ts, Cons c' ts')
      | c == c' -> case Lower ts ⊔ Lower ts' of
          Lower ts'' -> Cons c ts''
          _          -> Wildcard
      | otherwise -> Wildcard
    (StringLiteral s, StringLiteral s')
      | s == s' -> StringLiteral s
      | otherwise -> Wildcard
    (NumberLiteral n, NumberLiteral n')
      | n == n' -> NumberLiteral n
      | otherwise -> Wildcard
    (Wildcard, _) -> Wildcard
    (_, Wildcard) -> Wildcard
    (_, _) -> Wildcard

instance Galois (CP.Pow C.Term) Term where
  alpha = lub . fmap go
    where
      go (C.Cons c ts) = Cons c (fmap go ts)
      go (C.StringLiteral s) = StringLiteral s
      go (C.NumberLiteral s) = NumberLiteral s
  gamma = error "Infinite"

instance Show Term where
  show (Cons c ts) = show c ++ if null ts then "" else show ts
  show (StringLiteral s) = show s
  show (NumberLiteral n) = show n
  show Wildcard = "_"

instance Num Term where
  t1 + t2 = Cons "Add" [t1,t2]
  t1 - t2 = Cons "Sub" [t1,t2]
  t1 * t2 = Cons "Mul" [t1,t2]
  abs t = Cons "Abs" [t]
  signum t = Cons "Signum" [t]
  fromInteger = NumberLiteral . fromIntegral

instance Hashable Term where
  hashWithSalt s (Cons c ts) = s `hashWithSalt` (0::Int) `hashWithSalt` c `hashWithSalt` ts
  hashWithSalt s (StringLiteral t) = s `hashWithSalt` (1::Int) `hashWithSalt` t
  hashWithSalt s (NumberLiteral n) = s `hashWithSalt` (2::Int) `hashWithSalt` n
  hashWithSalt s Wildcard = s `hashWithSalt` (3::Int)

instance NFData Term where
  rnf t = case t of
    Cons c ts -> rnf c `seq` rnf ts
    StringLiteral s -> rnf s
    NumberLiteral n -> rnf n
    Wildcard -> ()

instance Arbitrary Term where
  arbitrary = do
    he <- choose (0,7)
    wi <- choose (0,4)
    arbitraryTerm he wi

arbitraryTerm :: Int -> Int -> Gen Term
arbitraryTerm 0 _ =
  oneof
    [ Cons <$> arbitrary <*> pure []
    , StringLiteral <$> arbitraryLetter
    , NumberLiteral <$> choose (0,9)
    , pure Wildcard
    ]
arbitraryTerm h w = do
  w' <- choose (0,w)
  c <- arbitrary
  fmap (Cons c) $ vectorOf w' $ join $
    arbitraryTerm <$> choose (0,h-1) <*> pure w

instance UpperBounded TermEnv where
  top = S.empty

instance Galois (CP.Pow C.TermEnv) TermEnv where
  alpha = lub . fmap (\(C.TermEnv e) -> S.fromList (M.toList (fmap alphaSing e)))
  gamma = undefined

-- prim :: (ArrowTry p, ArrowAppend p, IsTerm t p, IsTermEnv (AbstractTermEnv t) t p)
--      => StratVar -> [TermVar] -> p a t
-- prim f ps = undefined
  -- proc _ -> case f of
  --   "SSL_strcat" -> do
  --     args <- lookupTermArgs -< ps
  --     case args of
  --       [T.StringLiteral t1, T.StringLiteral t2] -> stringLiteral -< t1 `append` t2
  --       [T.Wildcard, _] -> wildcard -< ()
  --       [_, T.Wildcard] -> wildcard -< ()
  --       _ -> fail -< ()
  --   "SSL_newname" -> do
  --     args <- lookupTermArgs -< ps
  --     case args of
  --       [T.StringLiteral _] -> wildcard -< ()
  --       [T.Wildcard] -> wildcard -< ()
  --       _ -> fail -< ()
  --   _ -> error ("unrecognized primitive function: " ++ show f) -< ()
  -- where
  --   lookupTermArgs = undefined
      -- proc args -> do
      -- tenv <- getTermEnv -< ()
      -- case mapM (`M.lookup` tenv) args of
      --   Just t -> mapA matchTerm -< t
      --   Nothing -> fail <+> success -< [T.Wildcard | _ <- args]
-- {-# SPECIALISE prim :: StratVar -> [TermVar] -> Interp StratEnv TermEnv PowersetResult Term Term #-}
 
