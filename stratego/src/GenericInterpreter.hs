{-# LANGUAGE Arrows #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
module GenericInterpreter where

import           Prelude hiding ((.),id,all,sequence,curry, uncurry,fail,map)

import           Syntax (Closure(..),Strategy(..),Strat(..),StratEnv,StratVar,TermPattern,TermVar)
import qualified Syntax as S
import           TermEnv as Env
import           Utils

import           Control.Category
import           Control.Arrow hiding ((<+>))
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.Except as Exc
import           Control.Arrow.Reader
import           Control.Arrow.Utils(map)

import qualified Data.HashMap.Lazy as M
import           Data.Constructor
import           Data.Text(Text)
import           Data.Identifiable
import           Data.Label

import           Text.Printf
import           GHC.Exts(IsString(..))
-- import qualified Debug.Trace as Debug

-- | Generic interpreter for Stratego
eval :: (ArrowChoice c, ArrowFail e c, ArrowExcept () c,
          ArrowApply c, ArrowFix (c (Strat,t) t), Identifiable t, Show t, Show env,
          HasStratEnv c, IsTerm t c, IsTermEnv env t c, IsString e,
          Exc.Join t c, Exc.Join (t,[t]) c, Env.Join env c, Env.Join t c)
      => (Strat -> c t t)
eval = fix' $ \ev s0 -> case s0 of
    Match f _ -> proc t -> match -< (f,t)
    Build f _ -> proc _ -> build -< f

    Id _ -> id
    S.Fail _ -> proc _ -> throw -< ()
    Seq s1 s2 _ -> proc t1 -> do t2 <- ev s1 -< t1; ev s2 -< t2
    GuardedChoice s1 s2 s3 _ -> try' (ev s1) (ev s2) (ev s3)

    One s _  -> mapSubterms (one  (ev s))
    Some s _ -> mapSubterms (some (ev s))
    All s _  -> mapSubterms (all  (ev s))

    Scope xs s _ -> scope xs (ev s)
    Let bnds body _ -> let_ bnds body ev
    Call f ss ts _ -> proc t -> do
      senv <- readStratEnv -< ()
      case M.lookup f senv of
        Just (Closure strat senv') -> do
          let senv'' = if M.null senv' then senv else senv'
          args <- map lookupTermVarOrFail -< ts
          scope (strategyTermArguments strat)
                (invoke ev) -<< (strat, senv'', ss, args, t)
        Nothing -> failString -< printf "strategy %s not in scope" (show f)
    Apply body _ -> ev body

    Prim {} -> proc _ -> failString -< "We do not support primitive operations."
{-# INLINEABLE eval #-}

-- | Apply a strategy non-determenistically to one of the subterms.
one :: (ArrowChoice c, ArrowExcept () c, Exc.Join (t,[t]) c) => c t t -> c [t] [t]
one f = proc l -> case l of
  (t:ts) -> do
    (t',ts') <- first f <+> second (one f) -< (t,ts)
    returnA -< (t':ts')
  [] -> throw -< ()
{-# INLINEABLE one #-}

-- | Apply a strategy to as many subterms as possible (as long as the
-- strategy does not fail).
some :: (ArrowChoice c, ArrowExcept () c, Exc.Join (t,[t]) c) => c t t -> c [t] [t]
some f = go
  where
    go = proc l -> case l of
      (t:ts) -> do
        (t',ts') <- try' (first f) (second go') (second go) -< (t,ts)
        returnA -< t':ts'
      -- the strategy did not succeed for any of the subterms, i.e. some(s) fails
      [] -> throw -< ()
    go' = proc l -> case l of
      (t:ts) -> do
        (t',ts') <- try' (first f) (second go') (second go') -< (t,ts)
        returnA -< t':ts'
      [] -> returnA -< []
{-# INLINEABLE some #-}

-- | Apply a strategy to all subterms.
all :: ArrowChoice c => c x y -> c [x] [y]
all = map
{-# INLINEABLE all #-}

scope :: (Show env,IsTermEnv env t c, ArrowExcept e c, Env.Join env c, Exc.Join y c)
      => [TermVar] -> c x y -> c x y
scope [] s = s
scope vars s = proc t -> do
  oldEnv <- getTermEnv -< ()
  deleteTermVars -< vars
  finally
    (proc (t,_) -> s -< t)
    (proc (_,oldEnv) -> unionTermEnvs -< (vars,oldEnv))
    -< (t, oldEnv)
{-# INLINE scope #-}

-- | Let binding for strategies.
let_ :: (ArrowApply c, HasStratEnv c)
     => [(StratVar,Strategy)] -> Strat -> (Strat -> c t t) -> c t t
let_ ss body interp = proc a -> do
  let ss' = [ (v,Closure s' M.empty) | (v,s') <- ss ]
  senv <- readStratEnv -< ()
  localStratEnv (M.union (M.fromList ss') senv) (interp body) -<< a
{-# INLINE let_ #-}

-- | Strategy calls bind strategy variables and term variables.
invoke :: (ArrowChoice c, ArrowFail e c, ArrowApply c, IsString e, IsTermEnv env t c, HasStratEnv c, Env.Join t c)
       => (Strat -> c t t) -> c (Strategy, StratEnv, [Strat], [t], t) t
invoke ev = proc (Strategy { strategyStratArguments = formalStratArgs
                           , strategyTermArguments = formalTermArgs
                           , strategyBody = body
                           , strategyLabel = l
                           },
                  senv, actualStratArgs, actualTermArgs, t) -> do
    bindings -< zip formalTermArgs actualTermArgs
    let senv' = bindStratArgs (zip formalStratArgs actualStratArgs) senv
    case body of
      Scope vars b _ -> localStratEnv senv' (ev (Scope vars (Apply b l) l)) -<< t
      b -> localStratEnv senv' (ev (Apply b l)) -<< t
  where
    bindStratArgs :: [(StratVar,Strat)] -> StratEnv -> StratEnv
    bindStratArgs [] senv = senv
    bindStratArgs ((v,Call v' [] [] _) : ss) senv =
      case M.lookup v' senv of
        Just s -> M.insert v s (bindStratArgs ss senv)
        _ -> error $ "unknown strategy: " ++ show v'
    bindStratArgs ((v,s) : ss) senv =
        M.insert v (Closure (Strategy [] [] s (label s) (S.freeStratVars s)) senv) (bindStratArgs ss senv)
{-# INLINE invoke #-}

-- | Matches a pattern against the current term. Pattern variables are
-- bound in the term environment.
match :: (Show env, Show t, ArrowChoice c, ArrowApply c, ArrowExcept () c, IsTerm t c, IsTermEnv env t c, Env.Join t c)
      => c (TermPattern,t) t
match = proc (p,t) -> case p of
  S.As v p2 -> do
    t' <- match -< (S.Var v,t)
    match -< (p2,t')
  S.Var "_" ->
    returnA -< t
  S.Var x ->
    -- Stratego implements non-linear pattern matching, i.e., if a
    -- variable appears multiple times in a term pattern, the terms at
    -- these positions are compared for equality.
    lookupTermVar
      (proc (t',(x,t)) -> do
        t'' <- equal -< (t,t')
        insertTerm -< (x,t'')
        returnA -< t'')
      (proc (x,t) -> do
        insertTerm -< (x,t)
        returnA -< t)
      -< (x,(x,t))
  S.Cons c ts ->
    matchCons (zipWithA match) -< (c,ts,t)
  S.StringLiteral s ->
    matchString -< (s,t)
  S.NumberLiteral n ->
    matchNum -< (n,t)
  S.Explode c ts ->
    matchExplode
      (proc c' ->  match -< (c,c'))
      (proc ts' -> match -< (ts,ts')) -<< t
{-# INLINABLE match #-}

-- | Build a new term from a pattern. Variables are pattern are
-- replaced by terms in the current term environment.
build :: (ArrowChoice c, ArrowFail e c, IsString e, IsTerm t c, IsTermEnv env t c, Env.Join t c)
      => c TermPattern t
build = proc p -> case p of
  S.As _ _ -> fail -< "As-pattern in build is disallowed"
  S.Var x ->
    lookupTermVar
      (proc (term,_) -> returnA -< term)
      (proc (x,p) -> fail -< fromString ("unbound term variable " ++ show x ++ " in build pattern " ++ show p))
      -< (x,(x,p))
  S.Cons c ts -> do
    ts' <- map build -< ts
    buildCons -< (c,ts')
  S.StringLiteral s ->
    buildString -< s
  S.NumberLiteral n ->
    buildNum -< n
  S.Explode c ts -> do
    c'  <- build -< c
    ts' <- build -< ts
    buildExplode -< (c',ts')
{-# INLINABLE build #-}


---- Interface of the generic interpreter ----

-- | Arrow-based interface for matching and constructing terms.
class Arrow c => IsTerm t c | c -> t where
  -- | Match a term against a constructor and a list of subterms.
  matchCons :: c ([t'],[t]) [t] -> c (Constructor, [t'], t) t 

  -- | Match a term against a string literal.
  matchString :: c (Text,t) t

  -- | Match a term against a number literal.
  matchNum :: c (Int,t) t

  -- | Match a term against an explode pattern. The first strategy
  -- matches against the constructor, the second against the 'Cons'
  -- list of subterms.
  matchExplode :: c t t -> c t t -> c t t

  -- | Construct a term from a constructor and subterms.
  buildCons :: c (Constructor,[t]) t

  -- | Construct a term from a number literal.
  buildNum :: c Int t

  -- | Construct a term from a string literal.
  buildString :: c Text t

  -- | Convert a string literal and a 'Cons' list of subterms into a new term.
  buildExplode :: c (t,t) t

  -- | Checks if a given term is equal to another term and return one
  -- of the terms. If the terms are not equal, this operation fails.
  equal :: c (t,t) t

  -- | Map a strategy over the subterms of a given term.
  mapSubterms :: c [t] [t] -> c t t

type HasStratEnv c = ArrowReader StratEnv c


---- Helper functions ----

readStratEnv :: HasStratEnv c => c a StratEnv
readStratEnv = proc _ -> ask -< ()
{-# INLINE readStratEnv #-}

localStratEnv :: HasStratEnv c => StratEnv -> c a b -> c a b
localStratEnv senv f =  proc a -> local f -< (senv,a)
{-# INLINE localStratEnv #-}

-- | Fixpoint combinator used by Stratego.
fix' :: (ArrowFix (c (z,x) y), ArrowApply c) => ((z -> c x y) -> (z -> c x y)) -> (z -> c x y)
fix' f = curry (fix (uncurry . f . curry))
  where
    curry :: Arrow c => c (z,x) y -> (z -> c x y)
    curry g z = proc x -> g -< (z,x)

    uncurry :: ArrowApply c => (z -> c x y) -> c (z,x) y
    uncurry g = proc (z,x) -> g z -<< x
    {-# INLINE uncurry #-}
    {-# INLINE curry #-}
{-# INLINE fix' #-}

-- trace :: (Show t, Arrow c, IsTermEnv env t c, Show env) => Strat -> c t t -> c t t
-- trace s f = proc t -> do
--    env <- getTermEnv -< ()
--    () <- returnA -< Debug.trace (printf "%s: %s, %s" (show s) (show t) (show env)) ()
--    t' <- f -< t
--    env' <- getTermEnv -< ()
--    () <- returnA -< Debug.trace (printf "%s: %s, %s ---> %s, %s" (show s) (show t) (show env) (show t') (show env')) ()
--    returnA -< t'
