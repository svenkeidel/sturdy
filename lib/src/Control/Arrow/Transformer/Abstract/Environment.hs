{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
module Control.Arrow.Transformer.Abstract.Environment where

import Prelude hiding ((.),read,Maybe(..))

import Control.Category
import Control.Arrow
import Control.Arrow.Cont
import Control.Arrow.Const
import Control.Arrow.Transformer.Reader
import Control.Arrow.Reader as Reader
import Control.Arrow.State
import Control.Arrow.Store
import Control.Arrow.Fail
import Control.Arrow.Except
import Control.Arrow.Trans
import Control.Arrow.Environment as Env
import Control.Arrow.Closure as Cls
import Control.Arrow.Fix
import Control.Arrow.Order

import Data.Abstract.IntersectionSet (Set)
import Data.Abstract.Maybe
import qualified Data.Abstract.StrongMap as SM
import qualified Data.Abstract.Environment.Flat as FM
import Data.Abstract.Closure (Closure)
import qualified Data.Abstract.Closure as Abs

import Data.Order
import Data.Identifiable
import Data.Profunctor
import Data.Profunctor.Unsafe((.#))
import Data.Coerce

import GHC.Exts

newtype EnvT (env :: k1 -> k2 -> *) var val c x y = EnvT (ReaderT (env var val) c x y)
  deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowTrans,ArrowLift,ArrowLowerBounded, ArrowComplete z,
            ArrowState s, ArrowFail e, ArrowExcept e, ArrowStore var' val', ArrowConst k, ArrowRun, ArrowCont)

runEnvT :: EnvT env var val c x y -> c (env var val,x) y
runEnvT = coerce
{-# INLINE runEnvT #-}

runEnvT' :: (IsList (env var val), Item (env var val) ~ (var,val), Profunctor c) => EnvT env var val c x y -> c ([(var,val)],x) y
runEnvT' f = lmap (first fromList) (runEnvT f)
{-# INLINE runEnvT' #-}

instance (Identifiable var, UpperBounded val, ArrowChoice c, Profunctor c) => ArrowEnv var val (EnvT SM.Map var val c) where
  type Join y (EnvT SM.Map var val c) = ArrowComplete y c
  lookup (EnvT f) (EnvT g) = EnvT $ proc (var,x) -> do
    env <- Reader.ask -< ()
    case SM.lookup' var env of
      Just val        -> f -< (val,x)
      Nothing         -> g -< x
      JustNothing val -> (f -< (val,x)) <⊔> (g -< x)
  extend (EnvT f) = EnvT $ proc (var,val,x) -> do
    env <- Reader.ask -< ()
    Reader.local f -< (SM.insert var val env,x)
  {-# INLINE lookup #-}
  {-# INLINE extend #-}

instance (Identifiable var, Traversable val, Complete (val (Set var)), ArrowChoice c, Profunctor c) => ArrowEnv var (val (FM.Env var val)) (EnvT FM.Env var val c) where
  type Join y (EnvT FM.Env var val c) = ArrowComplete y c
  lookup (EnvT f) (EnvT g) = EnvT $ proc (var,x) -> do
    env <- Reader.ask -< ()
    case FM.lookup var env of
      Just val        -> f -< (val,x)
      Nothing         -> g -< x
      JustNothing val -> (f -< (val,x)) <⊔> (g -< x)
  extend (EnvT f) = EnvT $ proc (var,val,x) -> do
    env <- Reader.ask -< ()
    Reader.local f -< (FM.insert var val env,x)
  {-# INLINE lookup #-}
  {-# INLINE extend #-}

instance (Identifiable var, Traversable val, Complete (val (Set var)), ArrowChoice c, Profunctor c) => ArrowLetRec var (val (FM.Env var val)) (EnvT FM.Env var val c) where
  letRec (EnvT f) = EnvT $ proc (ls,x) -> do
    env <- Reader.ask -< ()
    Reader.local f -< (FM.insertRec ls env,x)
  {-# INLINE letRec #-}

instance (Identifiable expr, ArrowLowerBounded c, ArrowChoice c, Profunctor c) => ArrowClosure expr (Closure expr (env var val)) (EnvT env var val c) where
  type Join y (EnvT env var val c) = ArrowComplete y c
  closure = EnvT $ proc expr -> do
    env <- Reader.ask -< ()
    returnA -< Abs.closure expr env
  apply (EnvT f) = EnvT $ Abs.apply $ proc ((expr,env),x) ->
    Reader.local f -< (env,(expr,x))
  {-# INLINE closure #-}
  {-# INLINE apply #-}

instance (ArrowApply c, Profunctor c) => ArrowApply (EnvT env var val c) where
  app = EnvT (app .# first coerce)
  {-# INLINE app #-}

instance ArrowReader r c => ArrowReader r (EnvT env var val c) where
  ask = lift' Reader.ask
  local f = lift $ lmap (\(env,(r,x)) -> (r,(env,x))) (Reader.local (unlift f))
  {-# INLINE ask #-}
  {-# INLINE local #-}

type instance Fix (EnvT env var val c) x y = EnvT env var val (Fix c (env var val,x) y)
deriving instance ArrowFix (Underlying (EnvT env var val c) x y) => ArrowFix (EnvT env var val c x y)
