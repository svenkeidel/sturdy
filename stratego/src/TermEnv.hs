{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DefaultSignatures #-}
module TermEnv where

import Prelude hiding ((.),id,all,sequence,curry, uncurry,fail,map)

import Syntax (TermVar)

import Control.Arrow hiding ((<+>))
import Control.Arrow.Store(ArrowStore)
import qualified Control.Arrow.Store as Store
import Control.Arrow.State
import Control.Arrow.Fail
import Control.Arrow.Trans
import Control.Arrow.Utils
import Control.Arrow.Transformer.Value
import Control.Arrow.Transformer.Const
import Control.Arrow.Transformer.Static
import Control.Arrow.Transformer.Reader

import Data.Profunctor

import GHC.Exts ( Constraint, IsString(..) )


-- | Arrow-based interface for term environments. Because of the
-- dynamic scoping of stratego, term environments are more like
-- mutable state then conventional environments.
class Arrow c => IsTermEnv env term c | c -> env, env -> term where
  -- | Type class constraint used by the abstract instances to join arrow computations.
  type family Join y (c :: * -> * -> *) :: Constraint

  -- | Fetch the current term environment.
  getTermEnv :: c () env
  default getTermEnv :: ArrowState env c => c () env
  getTermEnv = get
  {-# INLINE getTermEnv #-}

  -- | Fetch the current term environment.
  putTermEnv :: c env ()
  default putTermEnv :: ArrowState env c => c env ()
  putTermEnv = put
  {-# INLINE putTermEnv #-}

  -- | Lookup a term in the given environment, the first continuation
  -- is called in case the term is in the environment and the second
  -- continuation otherwise.
  lookupTermVar :: Join y c => c (term,x) y -> c x y -> c (TermVar,x) y
  default lookupTermVar :: (ArrowStore TermVar term c, Join y c, Store.Join y c ~ Join y c) => 
    c (term,x) y -> c x y -> c (TermVar,x) y
  lookupTermVar = Store.read
  {-# INLINE lookupTermVar #-}

  -- | Insert a term into the given environment.
  insertTerm :: c (TermVar,term) ()
  default insertTerm :: (ArrowStore TermVar term c) => c (TermVar,term) ()
  insertTerm = Store.write
  {-# INLINE insertTerm #-}

  -- | Delete the specified variables from the given environment.
  deleteTermVars :: c [TermVar] ()

  unionTermEnvs :: c ([TermVar],env) ()

lookupTermVarOrFail :: (IsTermEnv env t c, ArrowFail e c, IsString e, Join t c) => c TermVar t
lookupTermVarOrFail = proc var ->
  lookupTermVar
    (proc (term,_) -> returnA -< term)
    (proc var -> fail -< fromString ("Unbound variable " ++ show var))
    -< (var, var)
{-# INLINE lookupTermVarOrFail #-}

-- | Add a list of bindings to the given environment.
bindings :: (Profunctor c, ArrowChoice c, IsTermEnv env t c) => c [(TermVar,t)] ()
bindings = void (map insertTerm)
{-# INLINE bindings #-}

deriving instance (Profunctor c, IsTermEnv env t c) => IsTermEnv env t (ConstT r c)
deriving instance (Profunctor c, IsTermEnv env t c) => IsTermEnv env t (ValueT val c)

instance (ArrowApply c, Profunctor c, IsTermEnv env t c) => IsTermEnv env t (ReaderT r c) where
  type Join x (ReaderT r c) = Join x c
  getTermEnv = lift' getTermEnv
  putTermEnv = lift' putTermEnv
  lookupTermVar (ReaderT f) (ReaderT g) = ReaderT $ proc (r,x) ->
    lookupTermVar (proc t -> f -< (r,t)) (proc e -> g -< (r,e)) -<< x
  insertTerm = lift' insertTerm
  deleteTermVars = lift' deleteTermVars
  unionTermEnvs = lift' unionTermEnvs

instance (Profunctor c, Applicative r, IsTermEnv env t c) => IsTermEnv env t (StaticT r c) where
  type Join x (StaticT r c) = Join x c
  getTermEnv = StaticT $ pure getTermEnv
  putTermEnv = StaticT $ pure putTermEnv
  lookupTermVar (StaticT f) (StaticT g) = StaticT $ lookupTermVar <$> f <*> g
  insertTerm = StaticT $ pure insertTerm
  deleteTermVars = StaticT $ pure deleteTermVars
  unionTermEnvs = lift' unionTermEnvs


