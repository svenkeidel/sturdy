{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module ConcreteInterpreter where

import           Prelude hiding ((.),fail)

import           GenericInterpreter as Generic
import           Syntax (TermPattern)
import qualified Syntax as S
import           Syntax hiding (Fail,TermPattern(..))
import           Utils
import qualified Concrete.TermEnvironment as TEnv

import           Control.Arrow
import           Control.Arrow.Closure as Cls
import           Control.Arrow.Except
import           Control.Arrow.Trans
import           Control.Arrow.Transformer.Concrete.Environment as SEnv
import           Control.Arrow.Transformer.Concrete.Except
import           Control.Arrow.Transformer.Concrete.Failure
import           Control.Arrow.Transformer.Value
import           Control.Category
import           Control.Monad (join)
import           Control.Monad.Reader (replicateM)

import qualified Data.Function as Function
import           Data.Profunctor
import           Data.Concrete.Closure
import           Data.Concrete.Error (Error)
import           Data.HashMap.Lazy(HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.Constructor
import           Data.Hashable
import           Data.String (IsString(..))
import           Data.Term (TermUtils(..))
import           Data.Text (Text)
import           Data.Label

import           Test.QuickCheck hiding (generate)

-- | Terms are either a constructor with subterms or a literal.
data Term
  = Cons Constructor [Term]
  | StringLiteral Text
  | NumberLiteral Int
  deriving (Eq)

type SEnv = HashMap StratVar Cls
newtype Cls = Cls (Closure Strategy SEnv)

type TermEnv = TEnv.TermEnv Term

-- | Concrete interpreter arrow give access to the strategy
-- environment, term environment, and handles anonymous exceptions.
type Interp a b =
  ValueT Term
   (SEnv.EnvT SEnv
    (TEnv.EnvT Term
     (ExceptT ()
      (FailureT String (->))))) a b

-- | Executes a concrete interpreter computation.
runInterp :: Interp a b -> StratEnv -> TermEnv -> a -> Error String (Error () (TermEnv,b))
runInterp f senv tenv t = run f (tenv, (senv', t))
  where
    senv' = M.fromList [ (var,Cls (Closure strat senv')) | (var,strat) <- M.toList senv]

-- | Concrete interpreter function.
eval :: LStrat -> LStratEnv -> TermEnv -> Term -> Error String (Error () (TermEnv,Term))
eval ls lsenv =
  let ?fixpointAlgorithm = Function.fix in
  let (s,senv) = generate $ (,) <$> ls <*> lsenv
  in runInterp (Generic.eval s) senv

-- Instances -----------------------------------------------------------------------------------------

instance (ArrowChoice c, ArrowExcept () c) => IsTerm Term (ValueT Term c) where
  matchCons matchSubterms = proc (c,ts,t) -> case t of
    Cons c' ts' | c == c' && eqLength ts ts' -> do
      ts'' <- matchSubterms -< (ts,ts')
      returnA -< Cons c ts''
    _ -> throw -< ()

  matchString = proc (s,t) -> case t of
    StringLiteral s'
      | s == s' -> returnA -< t
      | otherwise -> throw -< ()
    _ -> throw -< ()

  matchNum = proc (n,t) -> case t of
    NumberLiteral n'
      | n == n' -> returnA -< t
      | otherwise -> throw -< ()
    _ -> throw -< ()

  matchExplode matchCons' matchSubterms = proc t -> case t of
      Cons (Constructor c) ts -> do
        matchCons' -< (StringLiteral c)
        matchSubterms -< convertToList ts
        returnA -< t
      StringLiteral _ -> do
        matchSubterms -< convertToList []
        returnA -< t
      NumberLiteral _ -> do
        matchSubterms -< convertToList []
        returnA -< t

  buildCons = arr (uncurry Cons)
  buildString = arr StringLiteral
  buildNum = arr NumberLiteral

  buildExplode = proc (c,ts) -> case (c,go ts) of
    (StringLiteral c', Just ts') -> returnA -< Cons (Constructor c') ts'
    _                            -> throw -< ()
    where
      go l = case l of
        Cons "Cons" [x,tl] -> (x:) <$> go tl
        Cons "Nil" [] -> Just []
        _ -> Nothing

  equal = proc (t1,t2) ->
    case (t1,t2) of
      (Cons c ts, Cons c' ts')
          | c == c' && eqLength ts ts' -> do
          ts'' <- zipWithA equal -< (ts,ts')
          buildCons -< (c,ts'')
      (StringLiteral s, StringLiteral s')
          | s == s' -> success -< t1
      (NumberLiteral n, NumberLiteral n')
          | n == n' -> success -< t1
      (_,_) -> throw -< ()

  mapSubterms f = proc t ->
    case t of
      Cons c ts -> do
        ts' <- f -< ts
        buildCons -< (c,ts')
      StringLiteral {} -> returnA -< t
      NumberLiteral {} -> returnA -< t

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

instance ArrowClosure Strategy (Closure Strategy SEnv) c => ArrowClosure Strategy Cls (ValueT Term c) where
  type Join y Cls (ValueT Term c) = Cls.Join y (Closure Strategy SEnv) c
  closure = ValueT $ rmap Cls Cls.closure
  apply (ValueT f) = ValueT $ lmap (first (\(Cls cl) -> cl)) (Cls.apply f)

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

instance IsClosure Cls SEnv where
  mapEnvironment f (Cls cl) = Cls (fmap f cl)
  traverseEnvironment f (Cls cl) = Cls <$> traverse f cl

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
