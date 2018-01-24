{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GrammarSemantics where

import           Prelude hiding (fail,all,id,(.))
import           Result
import           Syntax (Strat(..),Constructor(..),TermVar,StratEnv,TermPattern)
import qualified Syntax as S
import           Interpreter

import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.Text (Text)
import           Data.Hashable
import           Data.Maybe

import           Control.Category
import           Control.Arrow hiding ((<+>))
import           Control.Arrow.Operations
import           Control.Arrow.Transformer.Power
import           Control.Arrow.Transformer.Deduplicate
import           WildcardSemantics (Term(..))

newtype TermRef = TermRef Int deriving (Eq,Hashable,Num)

-- All TermEnv (env,store,maxRef) satisfy
-- * forall x in env, env(x) in store
-- * maxRef not in store
newtype TermEnv = TermEnv (HashMap TermVar TermRef, HashMap TermRef Term, TermRef)

lookup :: (ArrowState TermEnv p, ArrowChoice p, Try p) => p TermVar (Maybe TermRef)
lookup = proc ref -> do
  TermEnv (env,_,_) <- getTermEnv -< ()
  returnA -< M.lookup ref env

deref :: (ArrowState TermEnv p, ArrowChoice p, Try p) => p TermRef Term
deref = proc ref -> do
  TermEnv (env,store,_) <- getTermEnv -< ()
  case M.lookup ref store of
    Just t -> returnA -< t
    Nothing -> returnA -< error "failed to resolve reference"

storeTerm :: ArrowState TermEnv p => p (TermRef,Term) ()
storeTerm = proc (ref,t) -> do
  TermEnv (env,store,maxRef) <- getTermEnv -< ()
  putTermEnv -< TermEnv (env, M.insert ref t store, maxRef)

newTerm :: ArrowState TermEnv p => p Term TermRef
newTerm = proc t -> do
  TermEnv (env,store,maxRef) <- getTermEnv -< ()
  let newRef = maxRef
  putTermEnv -< TermEnv (env,M.insert newRef t store, maxRef + 1)
  returnA -< newRef

eval' :: (ArrowChoice p, ArrowState TermEnv p, ArrowAppend p, Try p, Deduplicate p, ArrowApply p)
      => Int -> StratEnv -> Strat -> p TermRef TermRef
eval' 0 _ _ = proc _ ->
  fail <+> newTerm -< Wildcard
eval' i senv s0 = dedup $ case s0 of
  Id -> id
  S.Fail -> fail
  Seq s1 s2 -> eval' i senv s2 . eval' i senv s1 
  GuardedChoice s1 s2 s3 -> try (eval' i senv s1) (eval' i senv s2) (eval' i senv s3)
  One s -> lift (one (eval' i senv s))
  Some s -> lift (some (eval' i senv s))
  All s -> lift (all (eval' i senv s))
  Scope xs s -> scope xs (eval' i senv s)
  Match f -> undefined --proc t -> match -< (f,t)
  Build f -> undefined --proc _ -> build -< f
  Let bnds body -> let_ senv bnds body (eval' i)
  Call f ss ps -> call senv f ss ps (eval' (i-1))

-- match :: (ArrowChoice p, ArrowState TermEnv p, ArrowAppend p, Try p) => p (TermPattern,Term) Term
-- match = proc (p,t) -> case p of
--   S.Var "_" -> success -< t
--   S.Var x -> do
--     env <- getTermEnv -< ()
--     case M.lookup x env of
--       Just t' -> do
--         t'' <- equal -< (t,t')
--         putTermEnv -< M.insert x t'' env
--         success -< t''
--       Nothing -> do
--         putTermEnv -< M.insert x t env
--         fail <+> success -< t
--   S.Cons c ts -> case t of
--     Cons c' ts'
--       | c == c' && length ts == length ts' -> do
--           ts'' <- zipWith match -< (ts,ts')
--           success -< Cons c ts''
--       | otherwise -> fail -< ()
--     Wildcard -> do
--       ts'' <- zipWith match -< (ts,[Wildcard | _ <- ts])
--       fail <+> success -< Cons c ts''
--     _ -> fail -< ()
--   S.Explode c ts -> case t of
--     Cons (Constructor c') ts' -> do
--       match -< (c,StringLiteral c')
--       match -< (ts, convertToList ts')
--       success -< t
--     StringLiteral _ -> do
--       match -< (ts, convertToList [])
--       success -< t
--     NumberLiteral _ -> do
--       match -< (ts, convertToList [])
--       success -< t
--     Wildcard ->
--       (do
--         match -< (c,  Wildcard)
--         match -< (ts, Wildcard)
--         success -< t)
--       <+>
--       (do
--         match -< (ts, convertToList [])
--         success -< t)
--   S.StringLiteral s -> case t of
--     StringLiteral s'
--       | s == s' -> success -< t
--       | otherwise -> fail -< ()
--     Wildcard -> fail <+> success -< StringLiteral s
--     _ -> fail -< ()
--   S.NumberLiteral n -> case t of
--     NumberLiteral n'
--       | n == n' -> success -< t
--       | otherwise -> fail -< ()
--     Wildcard -> fail <+> success -< NumberLiteral n
--     _ -> fail -< ()

-- equal :: (ArrowChoice p, ArrowAppend p, Try p) => p (Term,Term) Term
-- equal = proc (t1,t2) -> case (t1,t2) of
--   (Cons c ts,Cons c' ts')
--     | c == c' && length ts == length ts' -> do
--       ts'' <- zipWith equal -< (ts,ts')
--       returnA -< Cons c ts''
--     | otherwise -> fail -< ()
--   (StringLiteral s, StringLiteral s')
--     | s == s' -> success -< t1
--     | otherwise -> fail -< ()
--   (NumberLiteral n, NumberLiteral n')
--     | n == n' -> success -< t1
--     | otherwise -> fail -< ()
--   (Wildcard, t) -> fail <+> success -< t
--   (t, Wildcard) -> fail <+> success -< t
--   (_,_) -> fail -< ()

-- build :: (ArrowChoice p, ArrowState TermEnv p, ArrowAppend p, Try p) => p TermPattern Term
-- build = proc p -> case p of
--   S.Var x -> do
--     env <- getTermEnv -< ()
--     case M.lookup x env of
--       Just t -> returnA -< t
--       Nothing -> fail <+> success -< Wildcard
--   S.Cons c ts -> do
--     ts' <- mapA build -< ts
--     returnA -< Cons c ts'
--   S.Explode c ts -> do
--     c' <- build -< c
--     case c' of
--       StringLiteral s -> do
--         ts' <- build -< ts
--         ts'' <- convertFromList -< ts'
--         case ts'' of
--           Just tl -> success -< Cons (Constructor s) tl
--           Nothing -> fail <+> returnA -< Wildcard
--       Wildcard -> fail <+> returnA -< Wildcard
--       _ -> fail -< ()
--   S.NumberLiteral n -> returnA -< NumberLiteral n
--   S.StringLiteral s -> returnA -< StringLiteral s

convertToList :: [Term] -> Term
convertToList ts = case ts of
  (x:xs) -> Cons "Cons" [x,convertToList xs]
  [] -> Cons "Nil" []

convertFromList :: (ArrowChoice p, Try p) => p Term (Maybe [Term])
convertFromList = proc t -> case t of
  Cons "Cons" [x,tl] -> do
    xs <- convertFromList -< tl
    returnA -< (x:) <$> xs
  Cons "Nil" [] ->
    returnA -< Just []
  Wildcard -> returnA -< Nothing
  _ -> fail -< ()

lift :: (Try p,ArrowChoice p,ArrowAppend p)
     => p (Constructor,[Term]) (Constructor,[Term])
     -> p TermRef TermRef
lift p = proc r -> do
  t <- deref -< r
  case t of
    Cons c ts -> do
      (c',ts') <- p -< (c,ts)
      returnA -< Cons c' ts'
    StringLiteral {} -> returnA -< t
    NumberLiteral {} -> returnA -< t
    Wildcard -> fail <+> success -< Wildcard

