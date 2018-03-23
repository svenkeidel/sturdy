{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Arrows #-}
module Control.Arrow.Transformer.Either(EitherArrow(..),liftEither) where

import Prelude hiding (id,lookup)

import Control.Arrow
import Control.Arrow.Class.Fail
import Control.Arrow.Class.Reader
import Control.Arrow.Class.State
import Control.Arrow.Deduplicate
import Control.Arrow.Try
import Control.Category
import Control.Monad (join)

import Data.Order

newtype EitherArrow e c x y = EitherArrow { runEitherArrow :: c x (Either e y) }

liftEither :: Arrow c => c x y -> EitherArrow e c x y
liftEither f = EitherArrow (f >>> arr Right)
{-# INLINE liftEither #-}

instance ArrowChoice c => Category (EitherArrow r c) where
  id = liftEither id
  EitherArrow f . EitherArrow g = EitherArrow $ g >>> right f >>^ join

instance ArrowChoice c => Arrow (EitherArrow r c) where
  arr f = liftEither (arr f)
  first (EitherArrow f) = EitherArrow $ first f >>^ injectRight
  second (EitherArrow f) = EitherArrow $ second f >>^ injectLeft

injectRight :: (Either e a,x) -> Either e (a,x)
injectRight (er,x) = case er of
  Left e -> Left e
  Right a -> Right (a,x)
{-# INLINE injectRight #-}

injectLeft :: (x, Either e a) -> Either e (x,a)
injectLeft (x,er) = case er of
  Left e -> Left e
  Right a -> Right (x,a)
{-# INLINE injectLeft #-}

instance ArrowChoice c => ArrowChoice (EitherArrow r c) where
  left (EitherArrow f) = EitherArrow $ left f >>^ commuteLeft
  right (EitherArrow f) = EitherArrow $ right f >>^ commuteRight

commuteLeft :: Either (Either e x) y -> Either e (Either x y)
commuteLeft e0 = case e0 of
  Left (Left e) -> Left e
  Left (Right x) -> Right (Left x)
  Right y -> Right (Right y)
{-# INLINE commuteLeft #-}

commuteRight :: Either x (Either e y) -> Either e (Either x y)
commuteRight e0 = case e0 of
  Left x -> Right (Left x)
  Right (Left e) -> Left e
  Right (Right x) -> Right (Right x)
{-# INLINE commuteRight #-}

instance (ArrowChoice c, ArrowApply c) => ArrowApply (EitherArrow e c) where
  app = EitherArrow $ first runEitherArrow ^>> app

instance (ArrowChoice c, ArrowState s c) => ArrowState s (EitherArrow e c) where
  getA = liftEither getA
  putA = liftEither putA

instance ArrowChoice c => ArrowFail e (EitherArrow e c) where
  failA = EitherArrow (arr Left)

instance (ArrowChoice c, ArrowReader r c) => ArrowReader r (EitherArrow e c) where
  askA = liftEither askA
  localA (EitherArrow f) = EitherArrow (localA f)

-- instance (ArrowChoice c, ArrowEnv x y env c) => ArrowEnv x y env (EitherArrow e c) where
--   lookup = liftEither lookup
--   getEnv = liftEither getEnv
--   extendEnv = liftEither extendEnv
--   localEnv (EitherArrow f) = EitherArrow (localEnv f)

instance ArrowChoice c => ArrowTry x y z (EitherArrow e c) where
  tryA (EitherArrow f) (EitherArrow g) (EitherArrow h) = EitherArrow $ proc x -> do
    e <- f -< x
    case e of
      Right y -> g -< y
      Left _ -> h -< x

instance ArrowChoice c => ArrowZero (EitherArrow () c) where
  zeroArrow = proc _ -> failA -< ()

instance ArrowChoice c => ArrowPlus (EitherArrow () c) where
  EitherArrow f <+> EitherArrow g = EitherArrow $ proc x -> do
    e <- f -< x
    case e of
      Right y -> returnA -< Right y
      Left _ -> g -< x

instance ArrowChoice c => ArrowDeduplicate (EitherArrow e c) where
  dedupA = returnA

deriving instance PreOrd (c x (Either e y)) => PreOrd (EitherArrow e c x y)
deriving instance LowerBounded (c x (Either e y)) => LowerBounded (EitherArrow e c x y)
deriving instance Complete (c x (Either e y)) => Complete (EitherArrow e c x y)
deriving instance CoComplete (c x (Either e y)) => CoComplete (EitherArrow e c x y)
deriving instance UpperBounded (c x (Either e y)) => UpperBounded (EitherArrow e c x y)
