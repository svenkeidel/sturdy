{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module TypedSemantics where

import           InterpreterArrow
import           ConcreteSemantics hiding (Term(..),TermEnv,eval)
import           Sort
import           Signature hiding (lub)
import qualified Signature as Sig
import           Syntax(Module,Strat,StratEnv,stratEnv,signature)
import           Utils

import           Data.Constructor
import           Data.Order hiding (Top,lub)
import qualified Data.Term as T
import           Data.Term(HasTerm(..))
import           Data.TermEnv
import           Data.Text(Text,pack)
import           Data.TypedResult
import           Data.Complete (Complete)
import qualified Data.Complete as C

import           Control.Arrow

import           Text.Printf

data Term
  = Cons Constructor [Term] Sort
  | StringLiteral Text
  | NumberLiteral Int
  deriving (Eq)
 
type TermEnv = ConcreteTermEnv Term

evalModule :: Module -> Strat -> (Term,TermEnv) -> TypedResult (Term,TermEnv)
evalModule module_ = eval (signature module_) (stratEnv module_)

eval :: Signature -> StratEnv -> Strat -> (Term,TermEnv) -> TypedResult (Term,TermEnv)
eval sig senv s = runInterp (eval' s) (sig, senv)

instance HasTerm Term (Interp (Signature,senv) s TypedResult) where
  matchTerm = arr $ \case
    Cons c ts _ -> T.Cons c ts
    StringLiteral s -> T.StringLiteral s
    NumberLiteral n -> T.NumberLiteral n
  {-# INLINE matchTerm #-}

  term = proc t0 -> case t0 of
    T.Cons "Cons" [x,xs] -> do
      t' <- case getSort xs of
              List t -> lub -< (getSort x,t)
              _ -> typeError -< "tail of the list is not of type list"
      xs' <- updateTag -< (xs,List t')
      returnA -< Cons "Cons" [x, xs'] (List t')
    T.Cons "Nil" [] ->
      returnA -< Cons "Nil" [] $ List Bottom
    T.Cons "Some" [x] ->
      returnA -< Cons "Some" [x] $ Option $ getSort x
    T.Cons "None" [] ->
     returnA -< Cons "None" [] $ Option Bottom
    T.Cons "" ts -> returnA -< Cons "" ts $ Tuple $ fmap getSort ts
    T.Cons c ts -> do
      sig <- getSignature -< ()
      case Sig.lookupType c sig of
        Just (Fun ss rs)
          | eqLength ss ts ->
              if and (zipWith (subtype sig) (fmap getSort ts) ss) 
              then returnA -< Cons c ts rs
              else typeError -< pack $ printf "constructor application not well typed: %s\nexpected arguments: %s\nbut got: %s" (show c) (show ss) (show (fmap getSort ts))
          | otherwise -> typeError -< pack $ "Wrong number of arguments to constructor: " ++ show c
        Nothing -> typeError -< pack $ "cannot find constructor: " ++ show c
    T.StringLiteral s -> returnA -< StringLiteral s
    T.NumberLiteral n -> returnA -< NumberLiteral n
    _ -> returnA -< error "Pattern match non exhaustive"

updateTag :: (HasSignature p, TypeError p,ArrowChoice p) => p (Term, Sort) Term
updateTag = proc x0 -> case x0 of
  (Cons "Some" [x] _, Option s) -> do
    x' <- updateTag -< (x,s)
    returnA -< Cons "Some" [x'] (Option s)
  (Cons "None" [] _, Option s) ->
    returnA -< Cons "None" [] (Option s)
  (Cons "Cons" [x,xs] _, List s) -> do
    x'  <- updateTag -< (x,s)
    xs' <- updateTag -< (xs,List s)
    returnA -< Cons "Cons" [x', xs'] (List s)
  (Cons "Nil" [] _, List s) -> returnA -< Cons "Nil" [] (List s)
  (Cons "" xs _, Tuple ss) -> do
    xs' <- zipWithA updateTag -< (xs,ss)
    returnA -< Cons "" xs' (Tuple ss)
  (t, s') -> do
    sig <- getSignature -< ()
    if Sig.subtype sig (getSort t) s'
      then returnA -< t
      else typeError -< pack $ printf "Expected term of sort %s, but got %s" (show s') (show (getSort t))
       
lub :: (Arrow c, HasSignature c) => c (Sort,Sort) Sort
lub = proc (s1,s2) -> do
  sig <- getSignature -< ()
  returnA -< undefined -- Sig.lub sig s1 s2

instance Show Term where
  show (Cons c ts s) = show c ++ (if null ts then "" else show ts) ++ ":" ++ show s
  show (StringLiteral s) = show s
  show (NumberLiteral n) = show n

getSort :: Term -> Sort
getSort t = case t of
  Cons _ _ s -> s
  StringLiteral _ -> Sort "String"
  NumberLiteral _ -> Sort "INT"
 
instance ArrowChoice c => PreOrd Term c where
  (⊑) = proc (t1,t2) -> case (t1,t2) of
    (Cons c ts _,Cons c' ts' _) -> do
      b <- (⊑) -< (ts,ts')
      returnA -< c == c' && b
    (StringLiteral s, StringLiteral s') -> returnA -< s == s'
    (NumberLiteral n, NumberLiteral n') -> returnA -< n == n'
    (_, _) -> returnA -< False

instance ArrowChoice c => PartOrd Term c

instance ArrowChoice c => Lattice (Complete Term) c where
  (⊔) = proc (t1,t2) -> case (t1,t2) of
    (C.Complete (Cons c ts t), C.Complete (Cons c' ts' _))
      | c == c' -> do
        ts'' <- zipWithA (⊔) -< (C.Complete <$> ts,C.Complete <$> ts')
        returnA -< Cons c <$> sequenceA ts'' <*> pure t
      | otherwise -> returnA -< C.Top
    (C.Complete (StringLiteral s), C.Complete (StringLiteral s'))
      | s == s' -> returnA -< C.Complete (StringLiteral s)
      | otherwise -> returnA -< C.Top
    (C.Complete (NumberLiteral n), C.Complete (NumberLiteral n'))
      | n == n' -> returnA -< C.Complete (NumberLiteral n)
      | otherwise -> returnA -< C.Top
    (_, _) -> returnA -< C.Top
