{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
module Data.TermEnv where

import           Prelude hiding ((.),id,map)
import           Syntax (TermVar)

import           Control.Category
import           Control.Arrow

class Arrow c => IsTermEnv env t c | c -> env, env -> t where
  getTermEnv :: c () env
  putTermEnv :: c env ()
  lookupTermVar :: c t t -> c () t -> c (TermVar,env) t
  insertTerm :: c (TermVar,t,env) env
  deleteTermVars :: c ([TermVar],env) env
  unionTermEnvs :: c ([TermVar],env,env) env

lookupTermVar' :: IsTermEnv env t c => c t t -> c () t -> c TermVar t
lookupTermVar' f g = proc v -> do
  env <- getTermEnv -< ()
  lookupTermVar f g -< (v,env)

insertTerm' :: IsTermEnv env t c => c (TermVar,t) ()
insertTerm' = proc (v,t) -> do
  env <- getTermEnv -< ()
  putTermEnv <<< insertTerm -< (v,t,env)

deleteTermVars' :: IsTermEnv env t c => c [TermVar] ()
deleteTermVars' = proc vs -> do
  env <- getTermEnv -< ()
  putTermEnv <<< deleteTermVars -< (vs,env)
