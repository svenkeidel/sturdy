{-# LANGUAGE Arrows #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
module GenericInterpreter where

import           Prelude hiding ((.),id,all,sequence,curry, uncurry,fail,map)

import           Syntax (Strategy(..),Strat(..),StratVar,TermPattern,TermVar)
import qualified Syntax as S
import           TermEnv as TEnv
import           Utils

import           Control.Category
import           Control.Arrow hiding ((<+>))
import           Control.Arrow.Fail as Fail
import           Control.Arrow.Fix
import           Control.Arrow.Environment as SEnv
import           Control.Arrow.Closure as Cls
import           Control.Arrow.Except as Exc
import           Control.Arrow.Utils(map)
import           Control.Arrow.LetRec

import           Data.Constructor
import           Data.Text(Text)
import           Data.Identifiable
import           Data.Label

import           Text.Printf
import           GHC.Exts(IsString(..))
-- import qualified Debug.Trace as Debug

-- | Generic interpreter for Stratego
eval :: (
  ?fixpointAlgorithm :: FixpointAlgorithm (Fix (c (Strat,t) t)),
  ArrowChoice c, ArrowFail e c, ArrowExcept () c,
  ArrowApply c, ArrowFix (c (Strat,t) t), Identifiable t, Show t, Show env,
  ArrowEnv StratVar cls c, ArrowLetRec StratVar cls c, ArrowClosure Strategy cls c,
  IsTerm t c, IsTermEnv env t c, IsString e,
  Exc.Join t c, Exc.Join (t,[t]) c, SEnv.Join t c, TEnv.Join env c, TEnv.Join t c, Cls.Join t cls c, Fail.Join t c
  ) => (Strat -> c t t)
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

  Scope xs s _ -> proc t -> scope (ev s) -< (xs,t)
  Let bnds body _ -> let_ bnds body ev
  Call f ss ts _ -> call s0 f ss ts ev
  Apply _ body _ -> ev body

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

scope :: (IsTermEnv env t c, ArrowChoice c, ArrowExcept e c, TEnv.Join env c, Exc.Join y c)
      => c x y -> c ([TermVar],x) y
scope s = proc (v0,x) -> case v0 of
  [] -> s -< x
  vars -> do
    oldEnv <- getTermEnv -< ()
    deleteTermVars -< vars
    finally
      (proc (t,_) -> s -< t)
      (proc (_,(vars,oldEnv)) -> unionTermEnvs -< (vars,oldEnv))
        -< (x, (vars,oldEnv))
{-# INLINE scope #-}

-- | Let binding for strategies.
let_ :: (ArrowClosure Strategy cls c, ArrowLetRec StratVar cls c, ArrowChoice c) => [(StratVar,Strategy)] -> Strat -> (Strat -> c t t) -> c t t
let_ bs body ev = proc t -> do
  let vars = fst <$> bs
  let strats = snd <$> bs
  cls <- map Cls.closure -< strats
  letRec (ev body) -< (zip vars cls,t)
{-# INLINE let_ #-}

-- | Strategy calls bind strategy variables and term variables.
call :: forall cls c env t e.
        (ArrowChoice c, ArrowExcept () c, ArrowFail e c, IsString e,
         ArrowEnv StratVar cls c, ArrowLetRec StratVar cls c, ArrowClosure Strategy cls c,
         ArrowApply c, IsTermEnv env t c,
         TEnv.Join env c, TEnv.Join t c, SEnv.Join t c, Exc.Join t c, Cls.Join t cls c, Fail.Join t c)
       => Strat -> StratVar -> [Strat] -> [TermVar] -> (Strat -> c t t) -> c t t
call cll f actualStratArgs termArgVars ev = proc t ->

  -- Lookup the strategy in the strategy environment.
  SEnv.lookup
    (proc (cls,t) -> do

       -- Close the strategy arguments in the call-site environment.
       sargs <- map (Cls.closure @Strategy @cls @c) -< [ Strategy [] [] s (label s) | s <- actualStratArgs]
       Cls.apply
         (proc (Strategy formalStratArgs formalTermArgs body _, (sargs,t)) -> do

            -- Extend the strategy environment with the strategy arguments.
            let stratArgs = zip formalStratArgs sargs
            SEnv.extend'
              (proc (formalTermArgs,body,t) -> do
                 -- Lookup the term arguments in the call-site term environment.
                 actualTermArgs <- map lookupTermVarOrFail -< termArgVars

                 -- Extend the term environment with the term arguments.
                 let termBindings = zip formalTermArgs actualTermArgs

                 case body of
                   -- If the body of the strategy is a scope, apply the fixpoint
                   -- algorithm inside the scope such that term variables don't leak.
                   Scope vars b l -> scope (bindings (ev (Apply cll b l)))
                     -<< (formalTermArgs ++ vars,(termBindings,t))
                   _ -> scope (bindings (ev (Apply cll body (label body))))
                     -<< (formalTermArgs,(termBindings,t))

              ) -< (stratArgs,(formalTermArgs,body,t))

         ) -< (cls,(sargs,t))
    )
    (proc _ -> failString -< printf "strategy %s not in scope" (show f))
    -< (f,t)

  -- senv <- readStratEnv -< ()
  -- case Env.lookup f senv of
  --   Just (Closure (Strategy formalStratArgs formalTermArgs body l) closureEnv) -> do
  --     -- let senv' = Env.fromList' [ (v,Closure (Strategy [] [] s (label s)) ())
  --     --                           | (v,s) <- zip formalStratArgs actualStratArgs] senv
  --     --             `Env.union` closureEnv

  --     actualTermArgs <- map lookupTermVarOrFail -< termArgVars
  --     let termBindings = zip formalTermArgs actualTermArgs
  --     -- case body of
  --     --   Scope vars b _ -> localStratEnv (scope (bindings (ev (Apply b l))))
  --     --     -<< (senv', (formalTermArgs ++ vars, (termBindings,t)))
  --     --   b -> localStratEnv (scope (bindings (ev (Apply b l))))
  --     --     -<< (senv', (formalTermArgs, (termBindings,t)))
  --   Nothing -> failString -< printf "strategy %s not in scope" (show f)
{-# INLINE call #-}

-- | Matches a pattern against the current term. Pattern variables are
-- bound in the term environment.
match :: (Show env, Show t, ArrowChoice c, ArrowApply c, ArrowExcept () c, IsTerm t c, IsTermEnv env t c, TEnv.Join t c)
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
build :: (ArrowChoice c, ArrowFail e c, IsString e, IsTerm t c, IsTermEnv env t c, TEnv.Join t c, Fail.Join t c)
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

---- Helper functions ----

-- | Fixpoint combinator used by Stratego.
fix' :: (?fixpointAlgorithm :: FixpointAlgorithm (Fix (c (z,x) y)), ArrowFix (c (z,x) y), ArrowApply c) => ((z -> c x y) -> (z -> c x y)) -> (z -> c x y)
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
