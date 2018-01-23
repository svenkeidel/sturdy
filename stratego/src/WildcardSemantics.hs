{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures -fno-warn-orphans #-}
module WildcardSemantics where

import           Prelude hiding (id,concat,sequence,(.),uncurry)

import qualified ConcreteSemantics as C
import           SharedSemantics
import           Soundness
import           Syntax hiding (Fail,TermPattern(..))
import           Utils
    
import           Control.Arrow
import           Control.Arrow.Apply
import           Control.Arrow.Try
import           Control.Arrow.Fix
import           Control.Arrow.Fail
import           Control.Arrow.Deduplicate
import           Control.Arrow.Utils (failA')
import           Control.Category
import           Control.DeepSeq
import           Control.Monad.Reader
import           Control.Monad.Result
import           Control.Monad.State

import           Data.Constructor
import           Data.Foldable (foldr')
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.Hashable
import           Data.Order
import           Data.GaloisConnection
import           Data.FreeCompletion
import           Data.Powerset hiding (size)
import qualified Data.Powerset as P
import qualified Data.AbstractPowerset as A
import           Data.Term
import           Data.TermEnv
import           Data.Text (Text)
import           Data.Result

import           Test.QuickCheck hiding (Result(..))
import           Text.Printf

data Term
    = Cons Constructor [Term]
    | StringLiteral Text
    | NumberLiteral Int
    | Wildcard
    deriving (Eq)

newtype TermEnv = TermEnv (HashMap TermVar Term) deriving (Show,Eq,Hashable)
newtype Interp a b = Interp (Kleisli (ReaderT (StratEnv,Int) (StateT TermEnv (ResultT A.Pow))) a b)
  deriving (Category,Arrow,ArrowChoice,ArrowApply,ArrowTry,ArrowZero,ArrowPlus,ArrowDeduplicate,PreOrd,Complete)

runInterp :: Interp a b -> Int -> StratEnv -> TermEnv -> a -> A.Pow (Result (b,TermEnv))
runInterp (Interp f) i senv tenv a = runResultT (runStateT (runReaderT (runKleisli f a) (senv,i)) tenv)

eval :: Int -> Strat -> StratEnv -> TermEnv -> Term -> A.Pow (Result (Term,TermEnv))
eval i s = runInterp (eval' s) i

liftK :: (a -> _ b) -> Interp a b
liftK f = Interp (Kleisli f)

emptyEnv :: TermEnv
emptyEnv = TermEnv M.empty

-- Instances -----------------------------------------------------------------------------------------
instance ArrowFail () Interp where
  failA = Interp failA

instance HasStratEnv Interp where
  readStratEnv = liftK (const (fst <$> ask))
  localStratEnv senv (Interp (Kleisli f)) = liftK (local (first (const senv)) . f)

instance IsTermEnv TermEnv Term Interp where
  getTermEnv = liftK (const get)
  putTermEnv = liftK (modify . const)
  lookupTermVar f g = proc (v,TermEnv env) ->
    case M.lookup v env of
      Just t -> f -< t
      Nothing ->
        (proc () -> do
          putTermEnv -< TermEnv (M.insert v Wildcard env)
          f -< Wildcard)
        ⊔ g
        -<< ()
  insertTerm = arr $ \(v,t,TermEnv env) -> TermEnv (M.insert v t env)
  deleteTermVars = arr $ \(vars,TermEnv env) -> TermEnv (foldr' M.delete env vars)
  unionTermEnvs = arr (\(vars,TermEnv e1,TermEnv e2) -> TermEnv (M.union e1 (foldr' M.delete e2 vars)))

instance IsTerm Term Interp where
  matchTermAgainstConstructor matchSubterms = proc (c,ts,t) -> case t of
    Cons c' ts' | c == c' && eqLength ts ts' -> do
      ts'' <- matchSubterms -< (ts,ts')
      returnA -< Cons c ts''
    Wildcard -> do
      ts'' <- matchSubterms -< (ts,[ Wildcard | _ <- [1..(length ts)] ])
      returnA ⊔ failA' -< Cons c ts''
    _ -> failA -< ()
  
  matchTermAgainstString = proc (s,t) -> case t of
    StringLiteral s'
      | s == s' -> returnA -< t
      | otherwise -> failA -< ()
    Wildcard ->
      returnA ⊔ failA' -< StringLiteral s
    _ -> failA' -< ()
  
  matchTermAgainstNumber = proc (n,t) -> case t of
    NumberLiteral n'
      | n == n' -> returnA -< t
      | otherwise -> failA -< ()
    Wildcard ->
      success ⊔ failA' -< NumberLiteral n
    _ -> failA' -< ()
  
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
          | otherwise -> failA -< ()
      (StringLiteral s, StringLiteral s')
          | s == s' -> success -< t1
          | otherwise -> failA -< ()
      (NumberLiteral n, NumberLiteral n')
          | n == n' -> success -< t1
          | otherwise -> failA -< ()
      (Wildcard, t) -> failA' ⊔ success -< t
      (t, Wildcard) -> failA' ⊔ success -< t
      (_,_) -> failA' -< ()
  
  convertFromList = proc (c,ts) -> case (c,go ts) of
    (StringLiteral c', Just ts'') -> returnA -< Cons (Constructor c') ts''
    (Wildcard, Just _)            -> failA' ⊔ success -< Wildcard
    (_,                Nothing)   -> failA' ⊔ success -< Wildcard
    _                             -> failA' -< ()
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
      Wildcard -> failA' ⊔ success -< Wildcard
  
  
  cons = arr (uncurry Cons)
  numberLiteral = arr NumberLiteral
  stringLiteral = arr StringLiteral

instance UpperBounded (Interp () Term) where
  top = proc () -> success ⊔ failA' -< Wildcard

instance ArrowFix Interp Term where
  fixA f z = proc x -> do
    i <- getFuel -< ()
    if i <= 0
    then top -< ()
    else localFuel (f (fixA f) z) -< (i-1,x)
    where
      getFuel = liftK (const (snd <$> ask))
      localFuel (Interp (Kleisli g)) = liftK $ \(i,a) -> local (second (const i)) (g a)

instance Soundness Interp where
  sound senv xs f g = forAll (choose (0,3)) $ \i ->
    let con :: A.Pow (Result (_,TermEnv))
        con = A.dedup $ alpha (fmap (\(x,tenv) -> C.runInterp f senv tenv x) xs)
        abst :: A.Pow (Result (_,TermEnv))
        abst = A.dedup $ runInterp g i senv (alpha (fmap snd xs)) (alpha (fmap fst xs))
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

instance Complete Term where
  t1 ⊔ t2 = case (t1,t2) of
    (Cons c ts, Cons c' ts')
      | c == c' -> case Lower ts ⊔ Lower ts' of
        Lower ts'' -> Cons c ts''
        _             -> Wildcard
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

instance Galois (P.Pow C.Term) Term where
  alpha = lub . fmap go
    where
      go (C.Cons c ts) = Cons c (fmap go ts)
      go (C.StringLiteral s) = StringLiteral s
      go (C.NumberLiteral s) = NumberLiteral s
  gamma = error "Infinite"

instance Show Term where
  show (Cons c ts) = show c ++ show ts
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

internal :: Arrow c => c (HashMap TermVar Term) (HashMap TermVar Term) -> c TermEnv TermEnv
internal f = arr TermEnv . f . arr (\(TermEnv e) -> e)

map :: ArrowChoice c => c Term Term -> c TermEnv TermEnv
map f = internal (arr M.fromList . mapA (second f) . arr M.toList)

dom :: HashMap TermVar t -> [TermVar]
dom = M.keys

instance PreOrd TermEnv where
  TermEnv env1 ⊑ TermEnv env2 =
    Prelude.all (\v -> M.lookup v env1 ⊑ M.lookup v env2) (dom env2)

instance Complete TermEnv where
  TermEnv env1' ⊔ TermEnv env2' = go (dom env1') env1' env2' M.empty
    where
      go vars env1 env2 env3 = case vars of
        (v:vs) -> case (M.lookup v env1,M.lookup v env2) of
          (Just t1,Just t2) -> go vs env1 env2 (M.insert v (t1⊔t2) env3)
          _                 -> go vs env1 env2 env3
        [] -> TermEnv env3

instance Galois (Pow C.TermEnv) TermEnv where
  alpha = lub . fmap (\(C.TermEnv e) -> TermEnv (fmap alphaSing e))
  gamma = undefined

instance (Eq x, Hashable x, Eq x', Hashable x', Galois (Pow x) x') => Galois (Pow (Result x)) (A.Pow (Result x')) where
  alpha = P.fromFoldable . fmap (fmap alphaSing)
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
 
