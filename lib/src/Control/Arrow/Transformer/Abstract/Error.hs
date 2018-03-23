{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Arrows #-}
module Control.Arrow.Transformer.Abstract.Error(ErrorArrow(..),liftError) where

import           Prelude hiding (id,(.),lookup)

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Environment
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.Reader
import           Control.Arrow.State

import           Data.Abstract.Error
import           Data.Order

newtype ErrorArrow e c x y = ErrorArrow { runErrorArrow :: c x (Error e y) }

liftError :: Arrow c => c x y -> ErrorArrow e c x y
liftError f = ErrorArrow (f >>> arr Success)
{-# INLINE liftError #-}

instance ArrowChoice c => Category (ErrorArrow r c) where
  id = liftError id
  ErrorArrow f . ErrorArrow g = ErrorArrow $ proc x -> do
    ey <- g -< x
    case ey of
      Bot -> returnA -< Bot
      Fail e -> returnA -< Fail e
      Success y -> f -< y

instance ArrowChoice c => Arrow (ErrorArrow r c) where
  arr f = liftError (arr f)
  first (ErrorArrow f) = ErrorArrow $ first f >>^ injectRight
  second (ErrorArrow f) = ErrorArrow $ second f >>^ injectLeft

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

instance ArrowChoice c => ArrowChoice (ErrorArrow r c) where
  left (ErrorArrow f) = ErrorArrow $ left f >>^ commuteLeft
  right (ErrorArrow f) = ErrorArrow $ right f >>^ commuteRight

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

instance (ArrowChoice c, ArrowApply c) => ArrowApply (ErrorArrow e c) where
  app = ErrorArrow $ first runErrorArrow ^>> app

instance (ArrowChoice c, ArrowState s c) => ArrowState s (ErrorArrow e c) where
  getA = liftError getA
  putA = liftError putA

instance ArrowChoice c => ArrowFail e (ErrorArrow e c) where
  failA = ErrorArrow (arr Fail)

instance (ArrowChoice c, ArrowReader r c) => ArrowReader r (ErrorArrow e c) where
  askA = liftError askA
  localA (ErrorArrow f) = ErrorArrow (localA f)

instance (ArrowChoice c, ArrowEnv x y env c) => ArrowEnv x y env (ErrorArrow e c) where
  lookup = liftError lookup
  getEnv = liftError getEnv
  extendEnv = liftError extendEnv
  localEnv (ErrorArrow f) = ErrorArrow (localEnv f)

instance (ArrowChoice c, ArrowFix x (Error e y) c) => ArrowFix x y (ErrorArrow e c) where
  fixA f = ErrorArrow (fixA (runErrorArrow . f . ErrorArrow))

deriving instance PreOrd (c x (Error e y)) => PreOrd (ErrorArrow e c x y)
deriving instance LowerBounded (c x (Error e y)) => LowerBounded (ErrorArrow e c x y)
deriving instance Complete (c x (Error e y)) => Complete (ErrorArrow e c x y)
deriving instance CoComplete (c x (Error e y)) => CoComplete (ErrorArrow e c x y)
deriving instance UpperBounded (c x (Error e y)) => UpperBounded (ErrorArrow e c x y)
