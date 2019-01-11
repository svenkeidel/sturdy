{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Environment where

import Prelude hiding (lookup,fail)

import Control.Arrow
import Control.Arrow.Fail
import Control.Arrow.Utils

import Data.String

import Text.Printf

import GHC.Exts (Constraint)


-- | Arrow-based interface for interacting with environments.
class Arrow c => ArrowEnv var val env c | c -> var, c -> val, c -> env where
  type family Join (c :: * -> * -> *) x y :: Constraint

  -- | Lookup a variable in the current environment. The first
  -- continuation is called if the variable is in the enviroment, the
  -- second if it is not.
  lookup :: (Join c ((val,x),x) y) => c (val,x) y -> c x y -> c (var,x) y
  -- | Retrieve the current environment.
  getEnv :: c () env
  -- | Extend an environment with a binding.
  extendEnv :: c (var,val,env) env
  -- | Run a computation with a modified environment.
  localEnv :: c x y -> c (env,x) y

-- | Simpler version of environment lookup.
lookup' :: (Join c ((val,var),var) val, Show var, IsString e, ArrowFail e c, ArrowEnv var val env c) => c var val
lookup' = proc var ->
  lookup
    (proc (val,_) -> returnA -< val)
    (proc var     -> fail    -< fromString $ printf "Variable %s not bound" (show var))
    -< (var,var)

-- | Run a computation in an extended environment.
extendEnv' :: ArrowEnv var val env c => c a b -> c (var,val,a) b
extendEnv' f = proc (x,y,a) -> do
  env <- getEnv -< ()
  env' <- extendEnv -< (x,y,env)
  localEnv f -< (env',a)

-- | Add a list of bindings to the given environment.
bindings :: (ArrowChoice c, ArrowEnv var val env c) => c ([(var,val)],env) env
bindings = fold ((\(env,(x,y)) -> (x,y,env)) ^>> extendEnv)
