{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Transformer.Concrete.Except(Except(..)) where

import Prelude hiding (id,(.),lookup,read)

import Control.Arrow
import Control.Arrow.Const
import Control.Arrow.Deduplicate
import Control.Arrow.Environment
import Control.Arrow.Fail
import Control.Arrow.Fix
import Control.Arrow.Lift
import Control.Arrow.Reader
import Control.Arrow.Store
import Control.Arrow.State
import Control.Arrow.Except
import Control.Category

import Data.Concrete.Error
import Data.Monoidal
import Data.Identifiable

-- | Arrow transformer that adds exceptions to the result of a computation
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

instance (ArrowChoice c, ArrowState s c) => ArrowState s (Except e c) where
  get = lift get
  put = lift put

instance (ArrowChoice c, ArrowStore var val lab c) => ArrowStore var val lab (Except e c) where
  read = lift read
  write = lift write

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

instance (ArrowChoice c, ArrowRead var val x (Error e y) c) => ArrowRead var val x y (Except e c) where
  read (Except f) (Except g) = Except $ read f g

instance (ArrowChoice c, ArrowWrite x y c) => ArrowWrite x y (Except e c) where
  write = lift write

type instance Fix x y (Except e c) = Except e (Fix x (Error e y) c)
instance (ArrowChoice c, ArrowFix x (Error e y) c) => ArrowFix x y (Except e c) where
  fix f = Except (fix (runExcept . f . Except))

instance ArrowChoice c => ArrowExcept x y e (Except e c) where
  tryCatch (Except f) (Except g) = Except $ proc x -> do
    e <- f -< x
    case e of
      Fail er -> g -< (x,er)
      Success y -> returnA -< Success y

  finally (Except f) (Except g) = Except $ proc x -> do
    _ <- f -< x
    g -< x

instance ArrowChoice c => ArrowTryCatch e x y z (Except e c) where
  tryCatchA (Except f) (Except g) (Except h) = Except $ f >>> toEither ^>> (h ||| g)

instance (Identifiable e, ArrowChoice c, ArrowDeduplicate c) => ArrowDeduplicate (Except e c) where
  dedup (Except f) = Except (dedup f)

instance (ArrowChoice c, ArrowConst r c) => ArrowConst r (Except e c) where
  askConst = lift askConst
