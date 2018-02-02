{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Transformer.Fail where

import           Prelude hiding (id,(.))

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Class.Fail
import           Control.Arrow.Class.Reader
import           Control.Arrow.Class.State
import           Control.Monad (join)

import           Data.Error

newtype ErrorArrow e c x y = ErrorArrow { runErrorArrow :: c x (Error e y)}

liftError :: Arrow c => c x y -> ErrorArrow e c x y
liftError f = ErrorArrow (f >>> arr Success)
{-# INLINE liftError #-}

instance ArrowChoice c => Category (ErrorArrow r c) where
  id = liftError id
  ErrorArrow f . ErrorArrow g = ErrorArrow $ g >>> toEither ^>> right f >>^ (fromEither >>> join)

instance ArrowChoice c => Arrow (ErrorArrow r c) where
  arr f = liftError (arr f)
  first (ErrorArrow f) = ErrorArrow $ first f >>^ injectRight
  second (ErrorArrow f) = ErrorArrow $ second f >>^ injectLeft

injectRight :: (Error e a,x) -> Error e (a,x)
injectRight (er,x) = case er of
  Error e -> Error e
  Success a -> Success (a,x)
{-# INLINE injectRight #-}

injectLeft :: (x, Error e a) -> Error e (x,a)
injectLeft (x,er) = case er of
  Error e -> Error e
  Success a -> Success (x,a)
{-# INLINE injectLeft #-}

instance ArrowChoice c => ArrowChoice (ErrorArrow r c) where
  left (ErrorArrow f) = ErrorArrow $ left f >>^ commuteLeft
  right (ErrorArrow f) = ErrorArrow $ right f >>^ commuteRight

commuteLeft :: Either (Error e x) y -> Error e (Either x y)
commuteLeft e0 = case e0 of
  Left (Error e) -> Error e
  Left (Success x) -> Success (Left x)
  Right y -> Success (Right y)
{-# INLINE commuteLeft #-}

commuteRight :: Either x (Error e y) -> Error e (Either x y)
commuteRight e0 = case e0 of
  Left x -> Success (Left x)
  Right (Error e) -> Error e
  Right (Success x) -> Success (Right x)
{-# INLINE commuteRight #-}

instance (ArrowChoice c, ArrowState s c) => ArrowState s (ErrorArrow e c) where
  getA = liftError getA
  putA = liftError putA

instance ArrowChoice c => ArrowFail e (ErrorArrow e c) where
  failA = ErrorArrow (arr Error)

instance (ArrowChoice c, ArrowReader r c) => ArrowReader r (ErrorArrow e c) where
  askA = liftError askA
  localA (ErrorArrow f) = ErrorArrow (localA f)
