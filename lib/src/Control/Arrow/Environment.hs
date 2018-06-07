{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Control.Arrow.Environment where

import Prelude hiding (lookup)

import Control.Arrow
import Control.Arrow.Fail
import Control.Arrow.Utils

import Text.Printf

-- | Arrow-based interface for querying environments.
class Arrow c => ArrowLookup var val y c where
  -- | Lookup a variable in the current environment. The first
  -- continuation is called if the variable is in the enviroment, the
  -- second if it is not.
  lookup :: c (val,a) y -> c a y -> c (var,a) y

-- | Simpler version of environment lookup.
lookup' :: (Show var, ArrowFail String c, ArrowLookup var val val c) => c var val
lookup' = proc var ->
  lookup
    (proc (val,_) -> returnA -< val)
    (proc var     -> failA   -< printf "Variable %s not bound" (show var))
    -< (var,var)

-- | Arrow-based interface for interacting with environments.
class Arrow c => ArrowEnv var val env c | c -> var, c -> val, c -> env where
  -- | Retrieve the current environment.
  getEnv :: c () env
  -- | Extend an environment with a binding.
  extendEnv :: c (var,val,env) env
  -- | Run a computation with a modified environment.
  localEnv :: c a b -> c (env,a) b

-- | Run a computation in an extended environment.
extendEnv' :: ArrowEnv var val env c => c a b -> c (var,val,a) b
extendEnv' f = proc (x,y,a) -> do
  env <- getEnv -< ()
  env' <- extendEnv -< (x,y,env)
  localEnv f -< (env',a)

-- | Add a list of bindings to the given environment.
bindings :: (ArrowChoice c, ArrowEnv var val env c) => c ([(var,val)],env) env
bindings = foldA ((\(env,(x,y)) -> (x,y,env)) ^>> extendEnv)
