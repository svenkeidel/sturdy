{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Transformer.Abstract.Environment where

import Prelude hiding ((.),read,Maybe(..))

import Control.Category
import Control.Arrow
import Control.Arrow.Const
import Control.Arrow.Transformer.Reader
import Control.Arrow.Reader as Reader
import Control.Arrow.State
import Control.Arrow.Store
import Control.Arrow.Fail
import Control.Arrow.Except
import Control.Arrow.Trans
import Control.Arrow.Environment as Env
import Control.Arrow.Fix
import Control.Arrow.Order

import Data.Order(UpperBounded)
import Data.Identifiable
import Data.Abstract.Maybe
import Data.Abstract.StrongMap (Map)
import qualified Data.Abstract.StrongMap as M
import Data.Profunctor
import Data.Profunctor.Unsafe((.#))
import Data.Coerce

newtype EnvT var val c x y = EnvT (ReaderT (Map var val) c x y)
  deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowTrans,ArrowLift,ArrowLowerBounded, ArrowComplete z,
            ArrowState s, ArrowFail e, ArrowExcept e, ArrowStore var' val', ArrowConst k, ArrowRun)

runEnvT :: EnvT var val c x y -> c (Map var val,x) y
runEnvT = coerce
{-# INLINE runEnvT #-}

runEnvT' :: (Profunctor c, Identifiable var) => EnvT var val c x y -> c ([(var,val)],x) y
runEnvT' f = lmap (first M.fromList) (runEnvT f)
{-# INLINE runEnvT' #-}

instance (Identifiable var, UpperBounded val, ArrowChoice c, Profunctor c) => ArrowEnv var val  (EnvT var val c) where
  type Join y (EnvT var val c) = ArrowComplete y c
  lookup (EnvT f) (EnvT g) = EnvT $ proc (var,x) -> do
    env <- Reader.ask -< ()
    case M.lookup' var env of
      Just val        -> f -< (val,x)
      Nothing         -> g -< x
      JustNothing val -> (f -< (val,x)) <⊔> (g -< x)
  extend (EnvT f) = EnvT $ proc (var,val,x) -> do
    env <- Reader.ask -< ()
    Reader.local f -< (M.insert var val env,x)

instance (Identifiable var, UpperBounded val, ArrowChoice c, Profunctor c) => ArrowClosure var val (Map var val) (EnvT var val c) where
  ask = EnvT Reader.ask
  local (EnvT f) = EnvT (Reader.local f)

instance (ArrowApply c, Profunctor c) => ArrowApply (EnvT var val c) where
  app = EnvT (app .# first coerce)

instance ArrowReader r c => ArrowReader r (EnvT var val c) where
  ask = lift' Reader.ask
  local f = lift $ lmap (\(env,(r,x)) -> (r,(env,x))) (Reader.local (unlift f))

type instance Fix (EnvT var val c) x y = EnvT var val (Fix c (Map var val,x) y)
deriving instance ArrowFix (Underlying (EnvT var val c) x y) => ArrowFix (EnvT var val c x y)
