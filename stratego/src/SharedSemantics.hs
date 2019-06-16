{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module SharedSemantics where

import           Prelude hiding ((.),id,all,sequence,curry, uncurry,fail)

import           Syntax hiding (Fail,TermPattern(..))
import           Syntax (TermPattern)
import qualified Syntax as S
import           TermEnv as Env
import           Utils

import           Control.Arrow hiding ((<+>))
import           Control.Arrow.Deduplicate
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.Except
import           Control.Arrow.Except as Exc
import           Control.Arrow.Reader
import           Control.Category

import qualified Data.HashMap.Lazy as M
import           Data.Constructor
import           Data.Text(Text)
import           Data.Identifiable

import           Text.Printf
import           GHC.Exts(IsString(..))

import qualified Debug.Trace as Debug

-- | Shared interpreter for Stratego
eval' :: (ArrowChoice c, ArrowFail e c, ArrowExcept () c,
          ArrowApply c, ArrowFix (Strat,t) t c, ArrowDeduplicate t t c, Identifiable t, Show t, Show env,
          HasStratEnv c, IsTerm t c, IsTermEnv env t c, IsString e,
          Exc.Join c (t,(t,())) t, Exc.Join c ((t,[t]),((t,[t]),())) (t,[t]), Exc.Join c (((t, env), t), ((t, env), ())) t,
          Env.Join c ((t, env), env) env, Env.Join c ((t, ()), ()) t, Env.Join c ((t, e), e) t,
          Exc.Join c ((((Strategy, StratEnv, t), env), t), (((Strategy, StratEnv, t), env), ())) t)
      => (Strat -> c t t)
eval' = fixA' $ \ev s0 -> dedup $ case s0 of
    Id -> id
    S.Fail -> proc _ -> throw -< ()
    Seq s1 s2 -> sequence (ev s1) (ev s2)
    GuardedChoice s1 s2 s3 -> guardedChoice (ev s1) (ev s2) (ev s3)
    One s -> mapSubterms (one (ev s))
    Some s -> mapSubterms (some (ev s))
    All s -> mapSubterms (all (ev s))
    Scope xs s -> scope xs (ev (Apply s))
    Match f -> proc t -> match -< (f,t)
    Build f -> proc _ -> build -< f
    Let bnds body -> let_ bnds body eval'
    Call f ss ts -> proc t -> do
      senv <- readStratEnv -< ()
      case M.lookup f senv of
        Just (Closure strat@(Strategy _ termParams _) senv') -> do
          let senv'' = if M.null senv' then senv else senv'
          args <- mapA (proc v -> ev (S.Build (S.Var v)) -<< t) -<< ts
          scope termParams (invoke ss args ev) -<< (strat, senv'', t)
        Nothing -> fail -< fromString $ printf "strategy %s not in scope" (show f)
    Prim {} -> undefined
    Apply body -> ev body
  -- where
  --   trace :: (Show t, Arrow c, IsTermEnv env t c, Show env) => Strat -> c t t -> c t t
  --   trace s f = proc t -> do
  --      env <- getTermEnv -< ()
  --      t' <- f -< Debug.trace (printf "%s -< %s, %s" (show s) (show t) (show env)) () `seq` t
  --      returnA -< Debug.trace (printf "%s <- %s -< %s, %s" (show t') (show s) (show t) (show env)) () `seq` t'

-- | Guarded choice executes the first strategy, if it succeeds the
-- result is passed to the second strategy, if it fails the original
-- input is passed to the third strategy.
guardedChoice :: (ArrowExcept () c, Exc.Join c (y,(x,())) z) => c x y -> c y z -> c x z -> c x z
guardedChoice = try'

-- | Sequencing of strategies is implemented with categorical composition.
sequence :: Category c => c x y -> c y z -> c x z
sequence f g = f >>> g

-- | Apply a strategy non-determenistically to one of the subterms.
one :: (ArrowChoice c, ArrowExcept () c, Exc.Join c ((t,[t]),((t,[t]),())) (t,[t])) => c t t -> c [t] [t]
one f = proc l -> case l of
  (t:ts) -> do
    (t',ts') <- first f <+> second (one f) -< (t,ts)
    returnA -< (t':ts')
  [] -> throw -< ()

-- | Apply a strategy to as many subterms as possible (as long as the
-- strategy does not fail).
some :: (ArrowChoice c, ArrowExcept () c, Exc.Join c ((t,[t]),((t,[t]),())) (t,[t])) => c t t -> c [t] [t]
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

-- | Apply a strategy to all subterms.
all :: ArrowChoice c => c x y -> c [x] [y]
all = mapA

scope :: (Show env,IsTermEnv env t c, ArrowExcept e c, Env.Join c ((t, env), env) env, Exc.Join c (((x, env), y), ((x, env), e)) y) => [TermVar] -> c x y -> c x y
scope [] s = s
scope vars s = proc t -> do
  oldEnv <- getTermEnv -< ()
  scopedEnv <- deleteTermVars -< (vars, oldEnv)
  putTermEnv -< scopedEnv
  finally
    (proc (t,_) -> s -< t)
    (proc (_,oldEnv) -> do
      newEnv <- getTermEnv -< ()
      putTermEnv <<< restoreEnv vars -< (oldEnv, newEnv)
    )
    -< (t, oldEnv)

  where restoreEnv []     = proc (_, env) -> returnA -< env
        restoreEnv (v:vs) = proc (oldEnv, env) -> do
          env' <- lookupTermVar
            (proc (t, env) -> insertTerm -< (v, t, env))
            (proc env      -> deleteTermVars -< ([v], env))
              -< (v, oldEnv, env)
          restoreEnv vs -< -- trace ("restored " ++ show (v, oldEnv, env, env'))
            (oldEnv, env')


localTermEnv :: (IsTermEnv env t c, ArrowExcept e c,  Exc.Join c (((x, env), y), ((x, env), e)) y) => c x y -> c (env,x) y
localTermEnv f = proc (newEnv,x) -> do
  oldEnv <- getTermEnv -< ()
  putTermEnv -< newEnv
  finally
    (proc (x,_) -> f -< x)
    (proc (_,env) -> putTermEnv -< env) -< (x,oldEnv)
  
-- | Let binding for strategies.
let_ :: (ArrowApply c, HasStratEnv c) => [(StratVar,Strategy)] -> Strat -> (Strat -> c t t) -> c t t
let_ ss body interp = proc a -> do
  let ss' = [ (v,Closure s' M.empty) | (v,s') <- ss ]
  senv <- readStratEnv -< ()
  localStratEnv (M.union (M.fromList ss') senv) (interp body) -<< a

-- | Strategy calls bind strategy variables and term variables.
invoke :: (Show t,Show env,ArrowChoice c, ArrowFail e c, ArrowApply c, IsString e, IsTermEnv env t c, HasStratEnv c, Env.Join c ((t, ()), ()) t)
     => [Strat]
     -> [t]
     -> (Strat -> c t t)
     -> c (Strategy, StratEnv, t) t
invoke actualStratArgs actualTermArgs ev = proc (Strategy formalStratArgs formalTermArgs body, senv, t) -> do
    tenv <- getTermEnv -< ()
    putTermEnv . bindings -< (zip formalTermArgs actualTermArgs, tenv)
    let senv' = bindStratArgs (zip formalStratArgs actualStratArgs) senv
    case body of
      Scope vars b -> localStratEnv senv' (ev (Scope vars (Apply b))) -<< t
      b -> localStratEnv senv' (ev (Apply b)) -<< t
  where
    bindStratArgs :: [(StratVar,Strat)] -> StratEnv -> StratEnv
    bindStratArgs [] senv = senv
    bindStratArgs ((v,Call v' [] []) : ss) senv =
      case M.lookup v' senv of
        Just s -> M.insert v s (bindStratArgs ss senv)
        _ -> error $ "unknown strategy: " ++ show v'
    bindStratArgs ((v,s) : ss) senv =
        M.insert v (Closure (Strategy [] [] s) senv) (bindStratArgs ss senv)

-- | Matches a pattern against the current term. Pattern variables are
-- bound in the term environment.
match :: (Show env, Show t,ArrowChoice c, ArrowApply c, ArrowExcept () c, IsTerm t c, IsTermEnv env t c, Env.Join c ((t, ()), ()) t)
      => c (TermPattern,t) t
match = proc (p,t) -> case p of
  S.As v p2 -> do
    t' <- match -< (S.Var v,t)
    match -< (p2,t')
  S.Var "_" ->
    returnA -< t
  S.Var x -> do
    -- Stratego implements non-linear pattern matching, i.e., if a
    -- variable appears multiple times in a term pattern, the terms at
    -- these positions are compared for equality.
    env <- getTermEnv -< ()
    lookupTermVar
      (proc (t',_) -> do t'' <- equal -< (t,t')
                         insertTerm' -< (x,t'')
                         returnA -< t'')
      (proc _ -> do insertTerm' -< (x,t)
                    returnA -< t) -<< (x,env,())
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

-- | Build a new term from a pattern. Variables are pattern are
-- replaced by terms in the current term environment.
build :: (ArrowChoice c, ArrowFail e c, IsString e, IsTerm t c, IsTermEnv env t c, Env.Join c ((t, e), e) t)
      => c TermPattern t
build = proc p -> case p of
  S.As _ _ -> fail -< "As-pattern in build is disallowed"
  S.Var x ->
    lookupTermVar' (fst ^>> returnA) fail -< (x,fromString ("unbound term variable " ++ show x ++ " in build statement " ++ show (Build p)))
  S.Cons c ts -> do
    ts' <- mapA build -< ts
    buildCons -< (c,ts')
  S.StringLiteral s ->
    buildString -< s
  S.NumberLiteral n ->
    buildNum -< n
  S.Explode c ts -> do
    c'  <- build -< c
    ts' <- build -< ts
    buildExplode -< (c',ts')

-- Interface of the shared interpreter

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

readStratEnv :: HasStratEnv c => c a StratEnv
readStratEnv = proc _ -> ask -< ()

localStratEnv :: HasStratEnv c => StratEnv -> c a b -> c a b
localStratEnv senv f =  proc a -> local f -< (senv,a)

-- | Fixpoint combinator used by Stratego.
fixA' :: (ArrowFix (z,x) y c, ArrowApply c) => ((z -> c x y) -> (z -> c x y)) -> (z -> c x y)
fixA' f = curry (fix (uncurry . f . curry))
  where
    curry :: Arrow c => c (z,x) y -> (z -> c x y)
    curry g z = proc x -> g -< (z,x)
    
    uncurry :: ArrowApply c => (z -> c x y) -> c (z,x) y
    uncurry g = proc (z,x) -> g z -<< x
