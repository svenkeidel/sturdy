{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Transformer.Concrete.Environment where

import           Prelude hiding ((.),read)

import           Control.Category

import           Control.Arrow
import           Control.Arrow.Const
import           Control.Arrow.Environment as Env
import           Control.Arrow.Closure as Closure
import           Control.Arrow.Except
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.Reader as Reader
import           Control.Arrow.State
import           Control.Arrow.Store
import           Control.Arrow.Trans

import           Control.Arrow.Transformer.Reader

import           Data.Concrete.Closure
import           Data.Identifiable
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.Profunctor
import           Data.Profunctor.Unsafe
import           Data.Coerce

-- | Arrow transformer that adds an environment to a computation.
newtype EnvT var val c x y = EnvT (ReaderT (HashMap var val) c x y)
  deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowLift,ArrowTrans,
            ArrowFail e,ArrowExcept e,ArrowState s,ArrowConst r,
            ArrowStore var' val', ArrowRun)

runEnvT :: EnvT var val c x y -> c (HashMap var val,x) y
runEnvT = coerce
{-# INLINE runEnvT #-}

runEnvT' :: (Profunctor c, Identifiable var) => EnvT var val c x y -> c ([(var,val)],x) y
runEnvT' f = lmap (first M.fromList) (runEnvT f)

instance (Identifiable var, ArrowChoice c, Profunctor c) => ArrowEnv var val (EnvT var val c) where
  type Join y (EnvT var val c) = ()
  lookup (EnvT f) (EnvT g) = EnvT $ proc (var,x) -> do
    env <- Reader.ask -< ()
    case M.lookup var env of
      Just val -> f -< (val,x)
      Nothing -> g -< x
  extend (EnvT f) = EnvT $ proc (var,val,x) -> do
    env <- Reader.ask -< ()
    Reader.local f -< (M.insert var val env, x)

instance (ArrowChoice c, Profunctor c) => ArrowClosure expr (Closure expr (HashMap var val)) (EnvT var val c) where
  type Join y (EnvT var val c) = ()
  closure = EnvT $ proc expr -> do
    env <- Reader.ask -< ()
    returnA -< Closure expr env
  apply (EnvT f) = EnvT $ proc (Closure expr env, x) ->
    Reader.local f -< (env,(expr,x))

instance (Identifiable var, IsClosure val (HashMap var val), ArrowChoice c, Profunctor c)
  => ArrowLetRec var val (EnvT var val c) where
  letRec (EnvT f) = EnvT $ proc (bindings,x) -> do
    env <- Reader.ask -< ()
    let env' = foldr (\(var,val) -> M.insert var (setEnvironment env' val)) env bindings
    Reader.local f -< (env',x)

instance (ArrowApply c,Profunctor c) => ArrowApply (EnvT var val c) where
  app = EnvT $ app .# first coerce

instance ArrowReader r c => ArrowReader r (EnvT var val c) where
  ask = lift' Reader.ask
  local (EnvT (ReaderT f)) = EnvT (ReaderT (lmap (\(env,(r,x)) -> (r,(env,x))) (Reader.local f)))

deriving instance ArrowFix (Underlying (EnvT var val c) x y) => ArrowFix (EnvT var val c x y)

type instance Fix (EnvT var val c) x y = EnvT var val (Fix c (HashMap var val,x) y)
