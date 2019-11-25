{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
module Control.Arrow.Closure where

import Control.Arrow
import Control.Arrow.Trans

import Data.Profunctor

import GHC.Exts

class (Arrow c, Profunctor c) => ArrowClosure expr cls c | cls -> expr where
  type Join y c :: Constraint

  -- | creates a non-recursive closure from expression.
  closure :: c expr cls

  -- | Apply a closure in its closed environment.
  apply :: Join y c => c (expr,x) y -> c (cls, x) y

  -- default lifting
  default closure :: (c ~ (t c'), ArrowLift t, ArrowClosure expr cls c') => c expr cls
  closure = lift' closure
  {-# INLINE closure #-}

class IsClosure cls env where
  mapEnvironment :: (env -> env) -> cls -> cls
  traverseEnvironment :: Applicative f => (env -> f env) -> cls -> f cls
  setEnvironment :: env -> cls -> cls
  setEnvironment env = mapEnvironment (const env)
  {-# INLINE setEnvironment #-}
