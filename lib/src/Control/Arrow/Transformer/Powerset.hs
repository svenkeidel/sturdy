{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Arrows #-}
module Control.Arrow.Transformer.Powerset(PowersetArrow(..),liftPowerset) where

import           Prelude hiding (id,lookup)

import           Control.Arrow
import           Control.Arrow.Class.Environment
import           Control.Arrow.Class.Fail
import           Control.Arrow.Class.Reader
import           Control.Arrow.Class.State
import           Control.Arrow.Deduplicate
import           Control.Category
import           Control.Monad (join)

import qualified Data.AbstractPowerset as A
import           Data.Order
import           Data.Sequence

newtype PowersetArrow c x y = PowersetArrow { runPowersetArrow :: c x (A.Pow y)}

liftPowerset :: Arrow c => c x y -> PowersetArrow c x y
liftPowerset f = PowersetArrow $ proc x -> do
  g <- f -< x
  returnA -< A.singleton g
{-# INLINE liftPowerset #-}

mapPow :: ArrowChoice c => c x y -> c (A.Pow x) (A.Pow y)
mapPow f = proc (A.Pow s) -> case viewl s of
  EmptyL -> returnA -< A.empty
  (x :< xs) -> do
    p <- f -< x
    A.Pow ps <- mapPow f -< A.Pow xs
    returnA -< A.Pow (p <| ps)

instance ArrowChoice c => Category (PowersetArrow c) where
  id = liftPowerset id
  PowersetArrow f . PowersetArrow g = PowersetArrow $ g >>> mapPow f >>^ join

instance ArrowChoice c => Arrow (PowersetArrow c) where
  arr f = liftPowerset (arr f)
  first (PowersetArrow f) = PowersetArrow $ first f >>^ \(pow,n) -> A.cartesian (pow, A.singleton n)
  second (PowersetArrow f) = PowersetArrow $ second f >>^ \(n,pow) -> A.cartesian (A.singleton n, pow)

instance ArrowChoice c => ArrowChoice (PowersetArrow c) where
  left (PowersetArrow f) = PowersetArrow $ left f >>^ commuteLeft
  right (PowersetArrow f) = PowersetArrow $ right f >>^ commuteRight

commuteLeft :: Either (A.Pow c) d -> A.Pow (Either c d)
commuteLeft e0 = case e0 of
  Left (A.Pow e) -> A.fromFoldable $ fmap Left e
  Right f -> A.singleton (Right f)
{-# INLINE commuteLeft #-}

commuteRight :: Either d (A.Pow c) -> A.Pow (Either d c)
commuteRight e0 = case e0 of
  Left e -> A.singleton (Left e)
  Right (A.Pow e) -> A.fromFoldable $ fmap Right e
{-# INLINE commuteRight #-}

instance (ArrowChoice c, ArrowApply c) => ArrowApply (PowersetArrow c) where
  app = PowersetArrow $ first runPowersetArrow ^>> app

instance (ArrowChoice c, ArrowReader r c) => ArrowReader r (PowersetArrow c) where
  askA = liftPowerset askA
  localA (PowersetArrow f) = PowersetArrow $ localA f

instance (ArrowChoice c, ArrowState s c) => ArrowState s (PowersetArrow c) where
  getA = liftPowerset getA
  putA = liftPowerset putA

instance (ArrowChoice c, ArrowFail e c) => ArrowFail e (PowersetArrow c) where
  failA = liftPowerset failA

instance (ArrowChoice c, ArrowEnv x y env c) => ArrowEnv x y env (PowersetArrow c) where
  lookup = liftPowerset lookup
  getEnv = liftPowerset getEnv
  extendEnv = liftPowerset extendEnv
  localEnv (PowersetArrow f) = PowersetArrow $ localEnv f

instance (ArrowChoice c, ArrowZero c) => ArrowZero (PowersetArrow c) where
  zeroArrow = liftPowerset zeroArrow

instance (ArrowChoice c, ArrowPlus c) => ArrowPlus (PowersetArrow c) where
  PowersetArrow f <+> PowersetArrow g = PowersetArrow (f <+> g)

instance (ArrowChoice c, ArrowDeduplicate c) => ArrowDeduplicate (PowersetArrow c) where
  dedupA (PowersetArrow f) = PowersetArrow (dedupA f)

deriving instance PreOrd (c x (A.Pow y)) => PreOrd (PowersetArrow c x y)
deriving instance LowerBounded (c x (A.Pow y)) => LowerBounded (PowersetArrow c x y)
deriving instance Complete (c x (A.Pow y)) => Complete (PowersetArrow c x y)
deriving instance CoComplete (c x (A.Pow y)) => CoComplete (PowersetArrow c x y)
deriving instance UpperBounded (c x (A.Pow y)) => UpperBounded (PowersetArrow c x y)
