{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module ConcreteSemantics where

import           Prelude hiding ((.),fail)

import           SharedSemantics
import           Syntax (TermPattern)
import qualified Syntax as S
import           Syntax hiding (Fail,TermPattern(..))
import           Utils

import           Control.Arrow
import           Control.Arrow.Deduplicate
import           Control.Arrow.Except
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.Reader
import           Control.Arrow.State
import           Control.Arrow.Transformer.Concrete.Except
import           Control.Arrow.Transformer.Concrete.Failure
import           Control.Arrow.Transformer.Reader
import           Control.Arrow.Transformer.State
import           Control.Category
import           Control.Monad (join)
import           Control.Monad.Reader (replicateM)

import           Data.Concrete.Error
import           Data.Concrete.Failure (Failure)
import           Data.Constructor
import           Data.Foldable (foldr')
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.Hashable
import           Data.String (IsString(..))
import           Data.Term (TermUtils(..))
import           Data.Text (Text)

import           Test.QuickCheck

-- | Terms are either a constructor with subterms or a literal.
data Term
  = Cons Constructor [Term]
  | StringLiteral Text
  | NumberLiteral Int
  deriving (Eq)

newtype TermEnv = TermEnv (HashMap TermVar Term) deriving (Show,Eq,Hashable)

-- | Concrete interpreter arrow give access to the strategy
-- environment, term environment, and handles anonymous exceptions.
newtype Interp a b = Interp (ReaderT StratEnv (StateT TermEnv (ExceptT () (FailureT String (->)))) a b)
  deriving (Category,Arrow,ArrowChoice,ArrowApply)

-- | Executes a concrete interpreter computation.
runInterp :: Interp a b -> StratEnv -> TermEnv -> a -> Failure String (Error () (TermEnv,b))
runInterp (Interp f) senv tenv t = runFailureT (runExceptT (runStateT (runReaderT f))) (tenv, (senv, t))

-- | Concrete interpreter function.
eval :: Strat -> StratEnv -> TermEnv -> Term -> Failure String (Error () (TermEnv,Term))
eval s = runInterp (eval' s)

-- Instances -----------------------------------------------------------------------------------------
deriving instance ArrowState TermEnv Interp
deriving instance ArrowReader StratEnv Interp
deriving instance ArrowExcept () Interp
deriving instance ArrowFix (Strat,Term) Term Interp
deriving instance ArrowDeduplicate Term Term Interp
deriving instance ArrowFail String Interp

instance HasStratEnv Interp where
  readStratEnv = Interp (const () ^>> ask)
  localStratEnv senv f = proc a -> local f -< (senv,a)

instance IsTermEnv TermEnv Term Interp where
  getTermEnv = get
  putTermEnv = put
  lookupTermVar f g = proc (v,TermEnv env,exc) ->
    case M.lookup v env of
      Just t -> f -< t
      Nothing -> g -< exc
  insertTerm = arr $ \(v,t,TermEnv env) ->
    TermEnv (M.insert v t env)
  deleteTermVars = arr $ \(vars,TermEnv env) ->
    TermEnv (foldr' M.delete env vars)
  unionTermEnvs = arr (\(vars, TermEnv e1, TermEnv e2) ->
    TermEnv (M.union e1 (foldr' M.delete e2 vars)))

instance IsTerm Term Interp where
  matchTermAgainstConstructor matchSubterms = proc (c,ts,t) -> case t of
    Cons c' ts' | c == c' && eqLength ts ts' -> do
      ts'' <- matchSubterms -< (ts,ts')
      returnA -< Cons c ts''
    _ -> throw -< ()

  matchTermAgainstString = proc (s,t) -> case t of
    StringLiteral s'
      | s == s' -> returnA -< t
      | otherwise -> throw -< ()
    _ -> throw -< ()

  matchTermAgainstNumber = proc (n,t) -> case t of
    NumberLiteral n'
      | n == n' -> returnA -< t
      | otherwise -> throw -< ()
    _ -> throw -< ()

  matchTermAgainstExplode matchCons matchSubterms = proc t -> case t of
      Cons (Constructor c) ts -> do
        matchCons -< (StringLiteral c)
        matchSubterms -< convertToList ts
        returnA -< t
      StringLiteral _ -> do
        matchSubterms -< convertToList []
        returnA -< t
      NumberLiteral _ -> do
        matchSubterms -< convertToList []
        returnA -< t
  
  equal = proc (t1,t2) ->
    case (t1,t2) of
      (Cons c ts, Cons c' ts')
          | c == c' && eqLength ts ts' -> do
          ts'' <- zipWithA equal -< (ts,ts')
          cons -< (c,ts'')
      (StringLiteral s, StringLiteral s')
          | s == s' -> success -< t1
      (NumberLiteral n, NumberLiteral n')
          | n == n' -> success -< t1
      (_,_) -> throw -< ()

  convertFromList = proc (c,ts) -> case (c,go ts) of
    (StringLiteral c', Just ts') -> returnA -< Cons (Constructor c') ts'
    _                            -> throw -< ()
    where
      go t = case t of
        Cons "Cons" [x,tl] -> (x:) <$> go tl
        Cons "Nil" [] -> Just []
        _ -> Nothing

  mapSubterms f = proc t ->
    case t of
      Cons c ts -> do
        ts' <- f -< ts
        cons -< (c,ts')
      StringLiteral {} -> returnA -< t
      NumberLiteral {} -> returnA -< t

  cons = arr (uncurry Cons)
  numberLiteral = arr NumberLiteral
  stringLiteral = arr StringLiteral

instance TermUtils Term where
  convertToList ts = case ts of
    (x:xs) -> Cons "Cons" [x,convertToList xs]
    []     -> Cons "Nil"  []
    
  size (Cons _ ts) = sum (size <$> ts) + 1
  size (StringLiteral _) = 1
  size (NumberLiteral _) = 1

  height (Cons _ []) = 1
  height (Cons _ ts) = maximum (height <$> ts) + 1
  height (StringLiteral _) = 1
  height (NumberLiteral _) = 1
  
instance Show Term where
  show (Cons c ts) = show c ++ if null ts then "" else show ts
  show (StringLiteral s) = show s
  show (NumberLiteral n) = show n

instance IsString Term where
  fromString = StringLiteral . fromString

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

instance Arbitrary Term where
  arbitrary = do
    h <- choose (0,7)
    w <- choose (0,4)
    arbitraryTerm h w

similar :: Gen (Term,Term)
similar = do
  ~[t1,t2] <- similarTerms 2 5 2 7
  return (t1,t2)

similarTerms :: Int -> Int -> Int -> Int -> Gen [Term]
similarTerms m h w similarity = do
  t <- arbitraryTerm h w
  replicateM m (go 1 t)
  where
    go :: Int -> Term -> Gen Term
    go i t = distribution (i,height t + similarity) (arbitraryTerm h w) $ case t of
       Cons c ts -> Cons c <$> traverse (go (i+1)) ts
       _ -> return t

similarTermPattern :: Term -> Int -> Gen TermPattern
similarTermPattern t0 similarity = go 0 t0
  where
    go :: Int -> Term -> Gen TermPattern
    go i t = distribution (i,height t + similarity) (S.Var <$> arbitrary) $ case t of
       Cons c ts -> S.Cons c <$> traverse (go (i+1)) ts
       StringLiteral s -> return $ S.StringLiteral s
       NumberLiteral n -> return $ S.NumberLiteral n

distribution :: (Int,Int) -> Gen a -> Gen a -> Gen a
distribution (p,n) a b = oneof (replicate p a ++ replicate (n-p) b)

arbitraryTerm :: Int -> Int -> Gen Term
arbitraryTerm 0 _ =
  oneof
    [ Cons <$> arbitrary <*> pure []
    , StringLiteral <$> arbitraryLetter
    , NumberLiteral <$> choose (0,9)
    ]
arbitraryTerm h w = do
  w' <- choose (0,w)
  c <- arbitrary
  fmap (Cons c) $ vectorOf w' $ join $
    arbitraryTerm <$> choose (0,h-1) <*> pure w

-- prim f ps = proc _ -> case f of
--   "strcat" -> do
--     tenv <- getTermEnv -< ()
--     case mapM (`M.lookup` tenv) ps of
--       Just [t1, t2] -> do
--         m <- matchTerm *** matchTerm -< (t1,t2)
--         case m of
--           (T.StringLiteral s1,T.StringLiteral s2) ->
--             T.stringLiteral -< s1 `Text.append` s2
--           _ -> fail -< ()
--       _ -> fail -< ()
--   "SSL_newname" -> do
--     tenv <- getTermEnv -< ()
--     case mapM (`M.lookup` tenv) ps of
--       Just [_] -> undefined -< ()
--       _ -> fail -< ()
--   _ -> error ("unrecognized primitive function: " ++ show f) -< ()
