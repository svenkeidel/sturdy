{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Transformer.Abstract.PropagateExcept(Except(..)) where

import Prelude hiding (id,(.),lookup)

import Control.Arrow
import Control.Arrow.Const
import Control.Arrow.Deduplicate
import Control.Arrow.Environment
import Control.Arrow.Fail
import Control.Arrow.Fix
import Control.Arrow.Lift
import Control.Arrow.Reader
import Control.Arrow.State
import Control.Category

import Data.Abstract.PropagateError
import Data.Order
import Data.Monoidal
import Data.Identifiable

-- | Describes computations that can fail. Usefull for implementing analyses for
-- languages that only propagate error and /do not/ handle it. For
-- languages that handle error, use 'Except'.
newtype Except e c x y = Except { runExcept :: c x (Error e y) }

instance ArrowLift (Except e) where
  lift f = Except (f >>> arr Success)

instance ArrowChoice c => Category (Except r c) where
  id = lift id
  Except f . Except g = Except $ g >>> toEither ^>> arr Fail ||| f

instance ArrowChoice c => Arrow (Except r c) where
  arr f = lift (arr f)
  first (Except f) = Except $ first f >>^ strength1
  second (Except f) = Except $ second f >>^ strength2

instance ArrowChoice c => ArrowChoice (Except r c) where
  left (Except f) = Except $ left f >>^ strength1
  right (Except f) = Except $ right f >>^ strength2
  Except f ||| Except g = Except (f ||| g)
  Except f +++ Except g = Except $ f +++ g >>^ from distribute

instance (ArrowChoice c, ArrowApply c) => ArrowApply (Except e c) where
  app = Except $ first runExcept ^>> app

instance (ArrowChoice c, ArrowLoop c) => ArrowLoop (Except e c) where
  loop (Except f) = Except $ loop $ proc (b,d) -> do
    e <- f -< (b,d)
    case e of
      Fail e' -> returnA -< (Fail e',d)
      Success (c,d') -> returnA -< (Success c,d')

instance (ArrowChoice c, ArrowState s c) => ArrowState s (Except e c) where
  get = lift get
  put = lift put

instance ArrowChoice c => ArrowFail e (Except e c) where
  fail = Except (arr Fail)

instance (ArrowChoice c, ArrowReader r c) => ArrowReader r (Except e c) where
  ask = lift ask
  local (Except f) = Except (local f)

instance (ArrowChoice c, ArrowEnv x y env c) => ArrowEnv x y env (Except e c) where
  lookup (Except f) (Except g) = Except $ lookup f g
  getEnv = lift getEnv
  extendEnv = lift extendEnv
  localEnv (Except f) = Except (localEnv f)

type instance Fix x y (Except e c) = Except e (Fix x (Error e y) c)
instance (ArrowChoice c, ArrowFix x (Error e y) c) => ArrowFix x y (Except e c) where
  fix = liftFix' runExcept Except

{-
There is no `ArrowExcept` instance for `Except` on purpose. This is how it would look like.

instance (ArrowChoice c, UpperBounded e, Complete (c (y,(x,e)) (Error e y))) => ArrowExcept x y e (Except e c) where
  tryCatchA (Except f) (Except g) = Except $ proc x -> do
    e <- f -< x
    case e of
      -- Since Fail ⊑ Success, 'Success' also represents the case 'f'
      -- could have failed. This means we also have to run the
      -- exception handler 'g' with all exceptions (represented by ⊤).
      Success y -> joined (arr Success) g -< (y,(x,top))
      Fail er   -> g -< (x,er)
-}

instance (Identifiable e, ArrowChoice c, ArrowDeduplicate x (Error e y) c) => ArrowDeduplicate x y (Except e c) where
  dedup (Except f) = Except (dedup f)

instance (ArrowChoice c, ArrowConst x c) => ArrowConst x (Except e c) where
  askConst = lift askConst

deriving instance PreOrd (c x (Error e y)) => PreOrd (Except e c x y)
deriving instance LowerBounded (c x (Error e y)) => LowerBounded (Except e c x y)
deriving instance Complete (c x (Error e y)) => Complete (Except e c x y)
deriving instance CoComplete (c x (Error e y)) => CoComplete (Except e c x y)
deriving instance UpperBounded (c x (Error e y)) => UpperBounded (Except e c x y)
