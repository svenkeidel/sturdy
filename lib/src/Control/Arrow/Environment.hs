{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Control.Arrow.Environment where

import Control.Arrow
import Control.Arrow.Utils

-- | Arrow-based interface for interacting with environments.
class Arrow c => ArrowEnv var val env c | c -> var, c -> val, c -> env where
  -- | Lookup a variable in the current environment.
  lookup :: c var val
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
