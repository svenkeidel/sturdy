{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Control.Arrow.Transformer.Reader(Reader(..)) where

import Prelude hiding (id,(.),lookup,read,fail)

import Control.Arrow
import Control.Arrow.Const
import Control.Arrow.Environment
import Control.Arrow.TryCatch
import Control.Arrow.Fail
import Control.Arrow.Fix
import Control.Arrow.Reader
import Control.Arrow.Store
import Control.Arrow.State
import Control.Arrow.Deduplicate
import Control.Arrow.Except
import Control.Arrow.Lift
import Control.Arrow.Writer
import Control.Arrow.Utils
import Control.Arrow.Abstract.Join
import Control.Category

import Data.Order hiding (lub)
import Data.Monoidal

-- Due to "Generalising Monads to Arrows", by John Hughes, in Science of Computer Programming 37.
newtype Reader r c x y = Reader { runReader :: c (r,x) y }

instance ArrowLift (Reader r) where
  lift f = Reader (pi2 >>> f)

instance Arrow c => Category (Reader r c) where
  id = lift id
  Reader f . Reader g = Reader $ (\(r,x) -> (r,(r,x))) ^>> f . second g

instance Arrow c => Arrow (Reader r c) where
  arr f = lift (arr f)
  first (Reader f) = Reader $ (\(r,(b,d)) -> ((r,b),d)) ^>> first f
  second (Reader f) = Reader $ (\(r,(b,d)) -> (b,(r,d))) ^>> second f
  Reader f &&& Reader g = Reader $ f &&& g
  Reader f *** Reader g = Reader $ (\(r,(b,d)) -> ((r,b),(r,d))) ^>> f *** g

instance ArrowChoice c => ArrowChoice (Reader r c) where
  left (Reader f) = Reader $ to distribute ^>> mmap id pi2  ^>> left f
  right (Reader f) = Reader $ to distribute ^>> mmap pi2 id ^>> right f
  Reader f +++ Reader g = Reader (to distribute ^>> f +++ g)
  Reader f ||| Reader g = Reader (to distribute ^>> f ||| g)

instance ArrowApply c => ArrowApply (Reader r c) where
  app = Reader $ (\(r,(Reader f,b)) -> (f,(r,b))) ^>> app

instance ArrowConst r c => ArrowConst r (Reader r' c) where
  askConst = lift askConst

instance Arrow c => ArrowReader r (Reader r c) where
  ask = Reader pi1
  local (Reader f) = Reader $ (\(_,(r,x)) -> (r,x)) ^>> f

instance ArrowStore var val lab c => ArrowStore var val lab (Reader r c) where
  read = lift read
  write = lift write

instance ArrowState s c => ArrowState s (Reader r c) where
  get = lift get
  put = lift put

instance ArrowWriter w c => ArrowWriter w (Reader r c) where
  tell = lift tell

instance ArrowFail e c => ArrowFail e (Reader r c) where
  fail = lift fail

instance ArrowEnv x y env c => ArrowEnv x y env (Reader r c) where
  lookup (Reader f) (Reader g) = Reader $ (\(r,(v,a)) -> (v,(r,a))) ^>> lookup ((\(v,(r,a)) -> (r,(v,a))) ^>> f) g
  getEnv = lift getEnv
  extendEnv = lift extendEnv
  localEnv (Reader f) = Reader ((\(r,(env,a)) -> (env,(r,a))) ^>> localEnv f)

instance ArrowRead var val (r,x) y c => ArrowRead var val x y (Reader r c) where
  read (Reader f) (Reader g) = Reader $ (\(r,(v,a)) -> (v,(r,a))) ^>> read ((\(v,(r,a)) -> (r,(v,a))) ^>> f) g

instance ArrowWrite var val c => ArrowWrite var val (Reader r c) where
  write = lift write

type instance Fix x y (Reader r c) = Reader r (Fix (r,x) y c)
instance ArrowFix (r,x) y c => ArrowFix x y (Reader r c) where
  fix f = Reader (fix (runReader . f . Reader))

instance ArrowExcept (r,x) y e c => ArrowExcept x y e (Reader r c) where
  tryCatch (Reader f) (Reader g) = Reader $ tryCatch f (from assoc ^>> g)
  finally (Reader f) (Reader g) = Reader $ finally f g

instance ArrowTryCatch (r,e) (r,x) (r,y) (r,z) c => ArrowTryCatch e x y z (Reader r c) where
  tryCatchA (Reader f) (Reader g) (Reader h) =
    Reader (tryCatchA (pi1 &&& f) (pi1 &&& g) (pi1 &&& h)) >>> pi2

instance ArrowDeduplicate c => ArrowDeduplicate (Reader r c) where
  dedup (Reader f) = Reader (dedup f)

instance ArrowJoin c => ArrowJoin (Reader r c) where
  joinWith lub (Reader f) (Reader g) = Reader $ (\(r,(x,y)) -> ((r,x),(r,y))) ^>> joinWith lub f g

instance ArrowConst x c => ArrowConst x (Reader r c) where
  askConst = lift askConst

deriving instance PreOrd (c (r,x) y) => PreOrd (Reader r c x y)
deriving instance LowerBounded (c (r,x) y) => LowerBounded (Reader r c x y)
deriving instance Complete (c (r,x) y) => Complete (Reader r c x y)
deriving instance CoComplete (c (r,x) y) => CoComplete (Reader r c x y)
deriving instance UpperBounded (c (r,x) y) => UpperBounded (Reader r c x y)
