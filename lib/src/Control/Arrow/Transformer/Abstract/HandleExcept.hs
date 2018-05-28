{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE GADTs #-}
module Control.Arrow.Transformer.Abstract.HandleExcept(Except(..)) where

import Prelude hiding (id,lookup,(.))

import Control.Arrow
import Control.Arrow.Deduplicate
import Control.Arrow.Environment
import Control.Arrow.Fail
import Control.Arrow.Lift
import Control.Arrow.Reader
import Control.Arrow.State
import Control.Arrow.Except
import Control.Arrow.Abstract.Join
import Control.Category

import Data.Abstract.HandleError
import Data.Monoidal
import Data.Order

newtype Except e c x y = Except { runExcept :: c x (Error e y)}

instance ArrowLift (Except e) where
  lift f = Except (f >>> arr Success)

instance (ArrowChoice c, ArrowJoin c, Complete e) => Category (Except e c) where
  id = lift id
  Except f . Except g = Except $ proc x -> do
    y <- g -< x
    case y of
      Success y' -> f -< y'
      Fail e -> returnA -< Fail e
      SuccessOrFail e y' -> do
        -- Ideally we would like to write '(returnA -< Fail e) ⊔ (f -< y)',
        -- however this is not possible, because the result type of
        -- 'f', 'UncertainError e z', is not 'Complete' because 'z' is not
        -- 'Complete'. However, in '(returnA -< Fail e) ⊔ (f -< y)' we
        -- actually never join to values of type 'z'.
        joinWith (\(Fail e) er -> case er of
            Success z           -> SuccessOrFail e z
            Fail e'             -> Fail (e ⊔ e')
            SuccessOrFail e' z -> SuccessOrFail (e ⊔ e') z)
          id f -< (Fail e,y')

instance (ArrowChoice c, ArrowJoin c, Complete e) => Arrow (Except e c) where
  arr f = lift (arr f)
  first (Except f) = Except $ first f >>^ strength1
  second (Except f) = Except $ second f >>^ strength2

instance (Complete e, ArrowJoin c, ArrowChoice c) => ArrowChoice (Except e c) where
  left (Except f) = Except $ left f >>^ strength1
  right (Except f) = Except $ right f >>^ strength2

instance (Complete e, ArrowJoin c, ArrowApply c, ArrowChoice c) => ArrowApply (Except e c) where
  app = Except $ first runExcept ^>> app

instance (Complete e, ArrowJoin c, ArrowChoice c, ArrowState s c) => ArrowState s (Except e c) where
  getA = lift getA
  putA = lift putA

instance (Complete e, ArrowJoin c, ArrowChoice c) => ArrowFail e (Except e c) where
  failA = Except $ arr Fail

instance (Complete e, ArrowJoin c, ArrowChoice c, ArrowReader r c) => ArrowReader r (Except e c) where
  askA = lift askA
  localA (Except f) = Except (localA f)

instance (Complete e, ArrowJoin c, ArrowChoice c, ArrowEnv x y env c) => ArrowEnv x y env (Except e c) where
  lookup = lift lookup
  getEnv = lift getEnv
  extendEnv = lift extendEnv
  localEnv (Except f) = Except (localEnv f)

instance (ArrowChoice c, Complete e, ArrowJoin c, Complete (c (y,(x,e)) (Error e y))) => ArrowExcept x y e (Except e c) where
  tryCatchA (Except f) (Except g) = Except $ proc x -> do
    e <- f -< x
    case e of
      Success y -> returnA -< Success y
      SuccessOrFail er y -> joined (arr Success) g -< (y,(x,er))
      Fail er -> g -< (x,er)

instance (Complete e, ArrowJoin c, ArrowChoice c) => ArrowDeduplicate (Except e c) where
  dedupA = returnA

deriving instance PreOrd (c x (Error e y)) => PreOrd (Except e c x y)
deriving instance LowerBounded (c x (Error e y)) => LowerBounded (Except e c x y)
deriving instance Complete (c x (Error e y)) => Complete (Except e c x y)
deriving instance CoComplete (c x (Error e y)) => CoComplete (Except e c x y)
deriving instance UpperBounded (c x (Error e y)) => UpperBounded (Except e c x y)
