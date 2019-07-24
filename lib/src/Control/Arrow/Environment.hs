{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Environment where

import Prelude hiding (lookup,fail,id)

import Control.Category
import Control.Arrow
import Control.Arrow.Fail

import Data.String
import Data.Profunctor

import Text.Printf

import GHC.Exts (Constraint)


-- | Arrow-based interface for interacting with environments.
class (Arrow c, Profunctor c) => ArrowEnv var val c | c -> var, c -> val where
  -- | Type class constraint used by the abstract instances to join arrow computations.
  type family Join (c :: * -> * -> *) x y :: Constraint

  -- TODO: Change type to lookup (Join c x y) => c (e,(val,s)) y -> c (e,s) y -> c (e,(var,s)) y
  -- | Lookup a variable in the current environment. If the
  -- environment contains a binding of the variable, the first
  -- continuation is called and the second computation otherwise.
  lookup :: Join c ((val,x),x) y => c (val,x) y -> c x y -> c (var,x) y

  -- | Extend an environment with a binding.
  extend :: c x y -> c (var,val,x) y

class ArrowEnv var val c => ArrowClosure var val env c | c -> env where
  -- | Retrieve the current environment.
  ask :: c () env

  -- | Run a computation with a modified environment.
  local :: c x y -> c (env,x) y

-- | Simpler version of environment lookup.
lookup' :: (Join c ((val,var),var) val, Show var, IsString e, ArrowFail e c, ArrowEnv var val c) => c var val
lookup' = lookup'' id

lookup'' :: (Join c ((val,var),var) y, Show var, IsString e, ArrowFail e c, ArrowEnv var val c) => c val y -> c var y
lookup'' f = proc var ->
  lookup
    (proc (val,_) -> f     -< val)
    (proc var     -> fail  -< fromString $ printf "Variable %s not bound" (show var))
    -< (var,var)

extend' :: (ArrowChoice c, ArrowEnv var val c) => c x y -> c ([(var,val)],x) y
extend' f = proc (l,x) -> case l of
  ((var,val):l') -> extend (extend' f) -< (var,val,(l',x))
  [] -> f -< x
