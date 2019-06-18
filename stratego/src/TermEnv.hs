{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module TermEnv where

import           Prelude hiding ((.),id,all,sequence,curry, uncurry,fail)

import           Syntax hiding (Fail,TermPattern(..))

import           Control.Arrow hiding ((<+>))
import           Control.Arrow.Fail
import           Control.Arrow.Trans
import           Control.Arrow.Utils
import           Control.Arrow.Transformer.Const
import           Control.Arrow.Transformer.Static
import           Control.Arrow.Transformer.Reader

import           Data.Profunctor

import           GHC.Exts(Constraint)
import           GHC.Exts(IsString(..))


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

lookupTermVar' :: (IsTermEnv env t c, Join c ((t,e),e) x) => c (t,e) x -> c e x -> c (TermVar,e) x
lookupTermVar' f g = proc (v,exc) -> do
  env <- getTermEnv -< ()
  lookupTermVar f g -< (v,env,exc)

lookupTermVarOrFail :: (IsTermEnv env t c, ArrowFail e c, IsString e, Join c ((t,e),e) t) => c TermVar t
lookupTermVarOrFail = proc v -> do
  env <- getTermEnv -< ()
  lookupTermVar (fst ^>> returnA) fail -< (v, env, fromString $ "Unbound variable " ++ show v)

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


deriving instance (Profunctor c, IsTermEnv env t c) => IsTermEnv env t (ConstT r c)
instance (ArrowApply c, Profunctor c, IsTermEnv env t c) => IsTermEnv env t (ReaderT r c) where
  type Join (ReaderT r c) ((t,e),e) x = Join c ((t,e),e) x
  getTermEnv = lift' getTermEnv
  putTermEnv = lift' putTermEnv
  emptyTermEnv = lift' emptyTermEnv
  lookupTermVar (ReaderT f) (ReaderT g) = ReaderT $ proc (r,x) -> lookupTermVar (proc t -> f -< (r,t)) (proc e -> g -< (r,e)) -<< x
  insertTerm = lift' insertTerm
  deleteTermVars = lift' deleteTermVars

instance (Profunctor c, Applicative r, IsTermEnv env t c) => IsTermEnv env t (StaticT r c) where
  type Join (StaticT r c) ((t,e),e) x = Join c ((t,e),e) x
  getTermEnv = StaticT $ pure getTermEnv
  putTermEnv = StaticT $ pure putTermEnv
  emptyTermEnv = StaticT $ pure emptyTermEnv
  lookupTermVar (StaticT f) (StaticT g) = StaticT $ lookupTermVar <$> f <*> g
  insertTerm = StaticT $ pure insertTerm
  deleteTermVars = StaticT $ pure deleteTermVars
