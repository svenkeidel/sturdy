{-# LANGUAGE MultiParamTypeClasses #-}
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
import           Utils

import           Control.Arrow hiding ((<+>))
import           Control.Arrow.Deduplicate
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.Trans
import           Control.Arrow.Except
import           Control.Arrow.Except as Exc
import           Control.Arrow.Reader
import           Control.Arrow.Transformer.Const
import           Control.Arrow.Transformer.Static
import           Control.Arrow.Transformer.Reader
import           Control.Category

import qualified Data.HashMap.Lazy as M
import           Data.Constructor
import           Data.Text(Text)
import           Data.Profunctor
import           Data.Identifiable

import           Text.Printf
import           GHC.Exts(IsString(..))
import qualified Debug.Trace as Debug

-- | Shared interpreter for Stratego
eval' :: (ArrowChoice c, ArrowFail e c, ArrowExcept () c,
          ArrowApply c, ArrowFix (Strat,t) t c, ArrowDeduplicate t t c, Identifiable t, Show t, Show env,
          HasStratEnv c, IsTerm t c, IsTermEnv env t c, IsString e,
          Exc.Join c (t,(t,())) t, Exc.Join c ((t,[t]),((t,[t]),())) (t,[t]))
      => (Strat -> c t t)
eval' = fixA' $ \ev s0 -> trace s0 $ dedup $ case s0 of
    Id -> id
    S.Fail -> proc _ -> throw -< ()
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
    Prim {} -> undefined
    Apply body -> ev body
  where
    trace :: (Show t, Arrow c, IsTermEnv env t c, Show env) => Strat -> c t t -> c t t
    trace s f = proc t -> do
       env <- getTermEnv -< ()
       t' <- f -< Debug.trace (printf "%s -< %s, %s" (show s) (show t) (show env)) () `seq` t
       returnA -< Debug.trace (printf "%s <- %s -< %s, %s" (show t') (show s) (show t) (show env)) () `seq` t'

-- | Guarded choice executes the first strategy, if it succeeds the
-- result is passed to the second strategy, if it fails the original
-- input is passed to the third strategy.
guardedChoice :: (ArrowExcept () c, Exc.Join c (x,(x,())) z) => c x y -> c y z -> c x z -> c x z
guardedChoice = try

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
        (t',ts') <- try (first f) (second go') (second go) -< (t,ts)
        returnA -< t':ts'
      -- the strategy did not succeed for any of the subterms, i.e. some(s) fails
      [] -> throw -< ()
    go' = proc l -> case l of
      (t:ts) -> do
        (t',ts') <- try (first f) (second go') (second go') -< (t,ts)
        returnA -< t':ts'
      [] -> returnA -< []

-- | Apply a strategy to all subterms.
all :: ArrowChoice c => c x y -> c [x] [y]
all = mapA

-- | Run a strategy in a term variable scope. The term variables of
-- the scope are deleted after the scope is exited.
scope :: IsTermEnv env t c => [TermVar] -> c x y -> c x y
scope vars s = proc t -> do
  env  <- getTermEnv      -< ()
  _    <- deleteTermVars' -< vars
  t'   <- s               -< t
  env' <- getTermEnv      -< ()
  putTermEnv <<< unionTermEnvs -< (vars,env,env')
  returnA -< t'

-- | Let binding for strategies.
let_ :: (ArrowApply c, HasStratEnv c) => [(StratVar,Strategy)] -> Strat -> (Strat -> c t t) -> c t t
let_ ss body interp = proc a -> do
  let ss' = [ (v,Closure s' M.empty) | (v,s') <- ss ]
  senv <- readStratEnv -< ()
  localStratEnv (M.union (M.fromList ss') senv) (interp body) -<< a

-- | Strategy calls bind strategy variables and term variables.
call :: (ArrowChoice c, ArrowFail e c, ArrowApply c, IsString e, IsTermEnv env t c, HasStratEnv c)
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
      let termArgs = (tenv,) <$> zip actualTermArgs formalTermArgs
          stratArgs = zip formalStratArgs actualStratArgs
      mapA bindTermArg -< termArgs
      let senv'' = bindStratArgs stratArgs (if M.null senv' then senv else senv')
      b <- localStratEnv senv'' (interp (Apply body)) -<< a
      tenv' <- getTermEnv -< ()
      putTermEnv <<< unionTermEnvs -< (formalTermArgs,tenv,tenv')
      returnA -< b
    Nothing -> fail -< fromString $ printf "strategy %s not in scope" (show f)
  where
    bindTermArg = proc (tenv,(actual,formal)) ->
      lookupTermVar (proc t -> do insertTerm' -< (formal,t); returnA -< (t))
                    (proc _ -> fail -< fromString $ "unbound term variable " ++ show actual ++ " in strategy call " ++ show (Call f actualStratArgs actualTermArgs))
        -<< (actual, tenv, ())
    {-# INLINE bindTermArg #-}

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
match :: (ArrowChoice c, ArrowApply c, ArrowExcept () c, IsTerm t c, IsTermEnv env t c)
      => c (TermPattern,t) t
match = proc (p,t) -> case p of
  S.As v p2 -> do
    t' <- match -< (S.Var v,t)
    match -< (p2,t')
  S.Var "_" ->
    returnA -< t
  S.Var x ->
    -- Stratego implements linear pattern matching, i.e., if a
    -- variable appears multiple times in a term pattern, the terms at
    -- these positions are compared for equality.
    lookupTermVar'
      (proc t' -> do t'' <- equal -< (t,t')
                     insertTerm' -< (x,t'')
                     returnA -< t'')
      (proc _ -> do insertTerm' -< (x,t)
                    returnA -< t) -<< (x,())
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
build :: (ArrowChoice c, ArrowFail e c, IsString e, IsTerm t c, IsTermEnv env t c)
      => c TermPattern t
build = proc p -> case p of
  S.As _ _ -> fail -< "As-pattern in build is disallowed"
  S.Var x ->
    lookupTermVar' returnA fail -< (x,fromString ("unbound term variable " ++ show x ++ " in build statement " ++ show (Build p)))
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

-- | Arrow-based interface for term environments. Because of the
-- dynamic scoping of stratego, term environments are more like
-- mutable state then conventional environments.
class Arrow c => IsTermEnv env t c | c -> env, env -> t where
  -- | Fetch the current term environment.
  getTermEnv :: c () env

  -- | Fetch the current term environment.
  putTermEnv :: c env ()
                
  emptyTermEnv :: c () ()

  -- | Lookup a term in the given environment, the first continuation
  -- is called in case the term is in the environment and the second
  -- continuation otherwise.
  lookupTermVar :: c t t -> c exc t -> c (TermVar,env,exc) t

  -- | Insert a term into the given environment.
  insertTerm :: c (TermVar,t,env) env

  -- | Delete the specified variables from the given environment.
  deleteTermVars :: c ([TermVar],env) env

  -- | Take the union of two term environments, where bindings of the
  -- first environment take precedence. Furthermore, the specified
  -- variables are removed from the second environment.
  unionTermEnvs :: c ([TermVar],env,env) env

lookupTermVar' :: IsTermEnv env t c => c t t -> c exc t -> c (TermVar,exc) t
lookupTermVar' f g = proc (v,exc) -> do
  env <- getTermEnv -< ()
  lookupTermVar f g -< (v,env,exc)

insertTerm' :: IsTermEnv env t c => c (TermVar,t) ()
insertTerm' = proc (v,t) -> do
  env <- getTermEnv -< ()
  putTermEnv <<< insertTerm -< (v,t,env)

deleteTermVars' :: IsTermEnv env t c => c [TermVar] ()
deleteTermVars' = proc vs -> do
  env <- getTermEnv -< ()
  putTermEnv <<< deleteTermVars -< (vs,env)

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

deriving instance (Profunctor c, IsTermEnv env t c) => IsTermEnv env t (ConstT r c)
instance (ArrowApply c, Profunctor c, IsTermEnv env t c) => IsTermEnv env t (ReaderT r c) where
  getTermEnv = lift' getTermEnv
  putTermEnv = lift' putTermEnv
  emptyTermEnv = lift' emptyTermEnv
  lookupTermVar (ReaderT f) (ReaderT g) = ReaderT $ proc (r,x) -> lookupTermVar (proc t -> f -< (r,t)) (proc e -> g -< (r,e)) -<< x
  insertTerm = lift' insertTerm
  deleteTermVars = lift' deleteTermVars
  unionTermEnvs = lift' unionTermEnvs

instance (Profunctor c, Applicative r, IsTermEnv env t c) => IsTermEnv env t (StaticT r c) where
  getTermEnv = StaticT $ pure getTermEnv
  putTermEnv = StaticT $ pure putTermEnv
  emptyTermEnv = StaticT $ pure emptyTermEnv
  lookupTermVar (StaticT f) (StaticT g) = StaticT $ lookupTermVar <$> f <*> g
  insertTerm = StaticT $ pure insertTerm
  deleteTermVars = StaticT $ pure deleteTermVars
  unionTermEnvs = StaticT $ pure unionTermEnvs
