{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
module SharedSemantics where

import           Prelude hiding (fail,(.),id,sum,flip,uncurry,all,sequence)

import           Syntax hiding (Fail,TermPattern(..))
import           Syntax (TermPattern)
import qualified Syntax as S
import           Utils

import           Control.Arrow
import           Control.Arrow.Deduplicate
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.Try
import           Control.Arrow.Utils hiding (mapA,zipWithA)
import           Control.Category

import qualified Data.HashMap.Lazy as M
import           Data.Term
import           Data.TermEnv
import           Data.Hashable

import           Text.Printf

-- Shared interpreter for Stratego
eval' :: (ArrowChoice c, ArrowTry c, ArrowPlus c, ArrowApply c, ArrowFix c t,
          ArrowFail () c, ArrowDeduplicate c, Eq t, Hashable t,
          HasStratEnv c, IsTerm t c, IsTermEnv env t c)
      => (Strat -> c t t)
eval' = fixA $ \ev s0 -> dedupA $ case s0 of
    Id -> id
    S.Fail -> failA'
    Seq s1 s2 -> sequence (ev s1) (ev s2)
    GuardedChoice s1 s2 s3 -> guardedChoice (ev s1) (ev s2) (ev s3)
    One s -> mapSubterms (one (ev s))
    Some s -> mapSubterms (some (ev s))
    All s -> mapSubterms (all (ev s))
    Scope xs s -> scope xs (ev s)
    Match f -> proc t -> match -< (f,t)
    Build f -> proc _ -> build -< f
    Let bnds body -> let_ bnds body eval'
    Call f ss ps -> call f ss ps ev

guardedChoice :: ArrowTry c => c x y -> c y z -> c x z -> c x z
guardedChoice = tryA

sequence :: Category c => c x y -> c y z -> c x z
sequence f g = f >>> g

one :: (ArrowChoice c, ArrowFail () c, ArrowTry c, ArrowPlus c) => c t t -> c [t] [t]
one f = proc l -> case l of
  (t:ts) -> do
    (t',ts') <- first f <+> second (one f) -< (t,ts)
    returnA -< (t':ts')
  [] -> failA -< ()

some :: (ArrowChoice c, ArrowFail () c, ArrowTry c) => c t t -> c [t] [t]
some f = go
  where
    go = proc l -> case l of
      (t:ts) -> do
        (t',ts') <- tryA (first f) (second go') (second go) -< (t,ts)
        returnA -< t':ts'
      -- the strategy did not succeed for any of the subterms, i.e. some(s) fails
      [] -> failA -< ()
    go' = proc l -> case l of
      (t:ts) -> do
        (t',ts') <- tryA (first f) (second go') (second go') -< (t,ts)
        returnA -< t':ts'
      [] -> returnA -< []

all :: ArrowChoice c => c x y -> c [x] [y]
all = mapA

scope :: IsTermEnv env t c => [TermVar] -> c x y -> c x y
scope vars s = proc t -> do
  env  <- getTermEnv      -< ()
  _    <- deleteTermVars' -< vars
  t'   <- s               -< t
  env' <- getTermEnv      -< ()
  putTermEnv <<< unionTermEnvs -< (vars,env,env')
  returnA -< t'

let_ :: (ArrowApply c, HasStratEnv c) => [(StratVar,Strategy)] -> Strat -> (Strat -> c t t) -> c t t
let_ ss body interp = proc a -> do
  let ss' = [ (v,Closure s' M.empty) | (v,s') <- ss ]
  senv <- readStratEnv -< ()
  localStratEnv (M.union (M.fromList ss') senv) (interp body) -<< a 

call :: (ArrowChoice c, ArrowFail () c, ArrowTry c, ArrowPlus c, ArrowApply c,
         IsTermEnv env t c, HasStratEnv c)
     => StratVar
     -> [Strat]
     -> [TermVar]
     -> (Strat -> c t t)
     -> c t t
call f actualStratArgs actualTermArgs interp = proc a -> do
  senv <- readStratEnv -< ()
  case M.lookup f senv of
    Just (Closure (Strategy formalStratArgs formalTermArgs body) senv') -> do
      tenv <- getTermEnv -< ()
      mapA bindTermArg -< zip actualTermArgs formalTermArgs
      let senv'' = bindStratArgs (zip formalStratArgs actualStratArgs)
                                 (if M.null senv' then senv else senv')
      b <- localStratEnv senv'' (interp body) -<< a
      tenv' <- getTermEnv -< ()
      putTermEnv <<< unionTermEnvs -< (formalTermArgs,tenv,tenv')
      returnA -< b
    Nothing -> error (printf "strategy %s not in scope" (show f)) -< ()
  where
    bindTermArg = proc (actual,formal) ->
      lookupTermVar' (proc t -> do insertTerm' -< (formal,t); returnA -< t) failA -<< actual
    {-# INLINE bindTermArg #-}

bindStratArgs :: [(StratVar,Strat)] -> StratEnv -> StratEnv
bindStratArgs [] senv = senv
bindStratArgs ((v,Call v' [] []) : ss) senv =
  case M.lookup v' senv of
    Just s -> M.insert v s (bindStratArgs ss senv)
    _ -> error $ "unknown strategy: " ++ show v'
bindStratArgs ((v,s) : ss) senv =
    M.insert v (Closure (Strategy [] [] s) senv) (bindStratArgs ss senv)
 
match :: (ArrowChoice c, ArrowTry c, ArrowApply c, IsTerm t c, IsTermEnv env t c)
      => c (TermPattern,t) t
match = proc (p,t) -> case p of
  S.As v p2 -> do
    t' <- match -< (S.Var v,t)
    match -< (p2,t')
  S.Var "_" ->
    success -< t
  S.Var x ->
    lookupTermVar'
      (proc t' -> do t'' <- equal -< (t,t'); insertTerm' -< (x,t''); returnA -< t'')
      (proc () -> do insertTerm' -< (x,t); returnA -< t) -<< x
  S.Cons c ts ->
    matchTermAgainstConstructor (zipWithA match) -< (c,ts,t)
  S.Explode c ts ->
    matchTermAgainstExplode
      (proc c' ->  match -< (c,c'))
      (proc ts' -> match -< (ts,ts')) -<< t
  S.StringLiteral s ->
    matchTermAgainstString -< (s,t)
  S.NumberLiteral n ->
    matchTermAgainstNumber -< (n,t)

build :: (ArrowChoice c, ArrowFail () c, ArrowTry c, IsTerm t c, IsTermEnv env t c)
      => c TermPattern t
build = proc p -> case p of
  S.As _ _ -> error "As-pattern in build is disallowed" -< ()
  S.Var x ->
    lookupTermVar' returnA failA -< x
  S.Cons c ts -> do
    ts' <- mapA build -< ts
    cons -< (c,ts')
  S.Explode c ts -> do
    c'  <- build -< c
    ts' <- build -< ts
    convertFromList -< (c',ts')
  S.NumberLiteral n ->
    numberLiteral -< n
  S.StringLiteral s ->
    stringLiteral -< s
