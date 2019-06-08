{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
module TermEnv where

import           Prelude hiding ((.),id,all,sequence,curry, uncurry,fail)

import           Syntax hiding (Fail,TermPattern(..))
import           Control.Arrow hiding ((<+>))
import            Control.Arrow.Utils

import           GHC.Exts(Constraint)


-- | Arrow-based interface for term environments. Because of the
-- dynamic scoping of stratego, term environments are more like
-- mutable state then conventional environments.
class Arrow c => IsTermEnv env t c | c -> env, env -> t where
  -- | Type class constraint used by the abstract instances to join arrow computations.
  type family Join (c :: * -> * -> *) x y :: Constraint

  -- | Fetch the current term environment.
  getTermEnv :: c () env

  -- | Fetch the current term environment.
  putTermEnv :: c env ()
                
  emptyTermEnv :: c () ()

  -- | Lookup a term in the given environment, the first continuation
  -- is called in case the term is in the environment and the second
  -- continuation otherwise.
  lookupTermVar :: (Join c ((t,e),e) x) => c (t,e) x -> c e x -> c (TermVar,env,e) x

  -- | Insert a term into the given environment.
  insertTerm :: c (TermVar,t,env) env

  -- | Delete the specified variables from the given environment.
  deleteTermVars :: c ([TermVar],env) env

  -- | Take the union of two term environments, where bindings of the
  -- first environment take precedence. Furthermore, the specified
  -- variables are removed from the second environment.
  unionTermEnvs :: c ([TermVar],env,env) env


lookupTermVar' :: (IsTermEnv env t c, Join c ((t,e),e) x) => c (t,e) x -> c e x -> c (TermVar,e) x
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

-- | Add a list of bindings to the given environment.
bindings :: (ArrowChoice c, IsTermEnv env t c) => c ([(TermVar,t)],env) env
bindings = fold ((\(env,(x,y)) -> (x,y,env)) ^>> insertTerm)
