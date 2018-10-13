{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE GADTs #-}
module Control.Arrow.Transformer.Abstract.HandleExcept(Except(..)) where

import Prelude hiding (id,lookup,(.),read)

import Control.Arrow
import Control.Arrow.Const
import Control.Arrow.Deduplicate
import Control.Arrow.Environment
import Control.Arrow.Fail
import Control.Arrow.Lift
import Control.Arrow.Reader
import Control.Arrow.State
import Control.Arrow.Store
import Control.Arrow.Except
import Control.Arrow.Fix
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
  get = lift get
  put = lift put

instance (Complete e, ArrowJoin c, ArrowChoice c, ArrowRead var val x (Error e y) c) => ArrowRead var val x y (Except e c) where
  read (Except f) (Except g) = Except $ read f g

instance (Complete e, ArrowJoin c, ArrowChoice c, ArrowWrite x y c) => ArrowWrite x y (Except e c) where
  write = lift write

instance (Complete e, ArrowJoin c, ArrowChoice c) => ArrowFail e (Except e c) where
  fail = Except $ arr Fail

instance (Complete e, ArrowJoin c, ArrowChoice c, ArrowReader r c) => ArrowReader r (Except e c) where
  ask = lift ask
  local (Except f) = Except (local f)

instance (Complete e, ArrowJoin c, ArrowChoice c, ArrowEnv x y env c) => ArrowEnv x y env (Except e c) where
  lookup (Except f) (Except g) = Except $ lookup f g
  getEnv = lift getEnv
  extendEnv = lift extendEnv
  localEnv (Except f) = Except (localEnv f)

instance (ArrowChoice c, Complete e, ArrowJoin c, Complete (c (y,(x,e)) (Error e y))) => ArrowExcept x y e (Except e c) where
  tryCatch (Except f) (Except g) = Except $ proc x -> do
    e <- f -< x
    case e of
      Success y -> returnA -< Success y
      SuccessOrFail er y -> joined (arr Success) g -< (y,(x,er))
      Fail er -> g -< (x,er)
  finally (Except f) (Except g) = Except $ proc x -> do
    e <- f -< x
    g -< x
    returnA -< e

type instance Fix x y (Except e c) = Except e (Fix x (Error e y) c)
instance (Complete e, ArrowJoin c, ArrowChoice c, ArrowFix x (Error e y) c) => ArrowFix x y (Except e c) where
  fix f = Except (fix (runExcept . f . Except))

instance (Complete e, ArrowJoin c, ArrowChoice c) => ArrowDeduplicate x y (Except e c) where
  dedup = returnA

instance (Complete e, ArrowJoin c, ArrowChoice c, ArrowConst r c) => ArrowConst r (Except e c) where
  askConst = lift askConst

instance (Complete e, ArrowJoin c, ArrowChoice c) => ArrowJoin (Except e c) where
  joinWith lub' (Except f) (Except g) = Except $ joinWith (\r1 r2 -> case (r1, r2) of
    (Success y1,          Success y2)          -> Success (y1 `lub'` y2)
    (Success y1,          SuccessOrFail e y2)  -> SuccessOrFail e (y1 `lub'` y2)
    (Success y,           Fail e)              -> SuccessOrFail e y
    (SuccessOrFail e y1,  Success y2)          -> SuccessOrFail e (y1 `lub'` y2)
    (SuccessOrFail e1 y1, SuccessOrFail e2 y2) -> SuccessOrFail (e1 ⊔ e2) (y1 `lub'` y2)
    (SuccessOrFail e1 y,  Fail e2)             -> SuccessOrFail (e1 ⊔ e2) y
    (Fail e,              Success y)           -> SuccessOrFail e y
    (Fail e1,             SuccessOrFail e2 y)  -> SuccessOrFail (e1 ⊔ e2) y
    (Fail e1,             Fail e2)             -> Fail (e1 ⊔ e2)
    ) f g

deriving instance PreOrd (c x (Error e y)) => PreOrd (Except e c x y)
deriving instance LowerBounded (c x (Error e y)) => LowerBounded (Except e c x y)
deriving instance Complete (c x (Error e y)) => Complete (Except e c x y)
deriving instance CoComplete (c x (Error e y)) => CoComplete (Except e c x y)
deriving instance UpperBounded (c x (Error e y)) => UpperBounded (Except e c x y)
