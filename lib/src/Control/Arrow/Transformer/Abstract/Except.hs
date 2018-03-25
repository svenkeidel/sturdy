{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Arrows #-}
module Control.Arrow.Transformer.Abstract.Except(Except(..)) where

import           Prelude hiding (id,(.),lookup)

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Environment
import           Control.Arrow.Fail
import           Control.Arrow.Lift
import           Control.Arrow.Fix
import           Control.Arrow.Reader
import           Control.Arrow.State

import           Data.Abstract.Error
import           Data.Order

newtype Except e c x y = Except { runExcept :: c x (Error e y) }

instance ArrowLift (Except e) where
  lift f = Except (f >>> arr Success)

instance ArrowChoice c => Category (Except r c) where
  id = lift id
  Except f . Except g = Except $ proc x -> do
    ey <- g -< x
    case ey of
      Bot -> returnA -< Bot
      Fail e -> returnA -< Fail e
      Success y -> f -< y

instance ArrowChoice c => Arrow (Except r c) where
  arr f = lift (arr f)
  first (Except f) = Except $ first f >>^ injectRight
  second (Except f) = Except $ second f >>^ injectLeft

injectRight :: (Error e a,x) -> Error e (a,x)
injectRight (er,x) = case er of
  Bot -> Bot
  Fail e -> Fail e
  Success a -> Success (a,x)
{-# INLINE injectRight #-}

injectLeft :: (x, Error e a) -> Error e (x,a)
injectLeft (x,er) = case er of
  Bot -> Bot
  Fail e -> Fail e
  Success a -> Success (x,a)
{-# INLINE injectLeft #-}

instance ArrowChoice c => ArrowChoice (Except r c) where
  left (Except f) = Except $ left f >>^ commuteLeft
  right (Except f) = Except $ right f >>^ commuteRight

commuteLeft :: Either (Error e x) y -> Error e (Either x y)
commuteLeft e0 = case e0 of
  Left Bot -> Bot
  Left (Fail e) -> Fail e
  Left (Success x) -> Success (Left x)
  Right y -> Success (Right y)
{-# INLINE commuteLeft #-}

commuteRight :: Either x (Error e y) -> Error e (Either x y)
commuteRight e0 = case e0 of
  Left x -> Success (Left x)
  Right Bot -> Bot
  Right (Fail e) -> Fail e
  Right (Success x) -> Success (Right x)
{-# INLINE commuteRight #-}

instance (ArrowChoice c, ArrowApply c) => ArrowApply (Except e c) where
  app = Except $ first runExcept ^>> app

instance (ArrowChoice c, ArrowState s c) => ArrowState s (Except e c) where
  getA = lift getA
  putA = lift putA

instance ArrowChoice c => ArrowFail e (Except e c) where
  failA = Except (arr Fail)

instance (ArrowChoice c, ArrowReader r c) => ArrowReader r (Except e c) where
  askA = lift askA
  localA (Except f) = Except (localA f)

instance (ArrowChoice c, ArrowEnv x y env c) => ArrowEnv x y env (Except e c) where
  lookup = lift lookup
  getEnv = lift getEnv
  extendEnv = lift extendEnv
  localEnv (Except f) = Except (localEnv f)

instance (ArrowChoice c, ArrowFix x (Error e y) c) => ArrowFix x y (Except e c) where
  fixA f = Except (fixA (runExcept . f . Except))

deriving instance PreOrd (c x (Error e y)) => PreOrd (Except e c x y)
deriving instance LowerBounded (c x (Error e y)) => LowerBounded (Except e c x y)
deriving instance Complete (c x (Error e y)) => Complete (Except e c x y)
deriving instance CoComplete (c x (Error e y)) => CoComplete (Except e c x y)
deriving instance UpperBounded (c x (Error e y)) => UpperBounded (Except e c x y)