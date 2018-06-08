{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Control.Arrow.Transformer.Reader(Reader(..)) where

import Prelude hiding (id,(.),lookup,read)

import Control.Arrow
import Control.Arrow.Environment
import Control.Arrow.Fail
import Control.Arrow.Fix
import Control.Arrow.Reader
import Control.Arrow.State
import Control.Arrow.Store
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

instance Arrow c => ArrowReader r (Reader r c) where
  askA = Reader pi1
  localA (Reader f) = Reader $ (\(_,(r,x)) -> (r,x)) ^>> f

instance ArrowState s c => ArrowState s (Reader r c) where
  getA = lift getA
  putA = lift putA

instance ArrowWriter w c => ArrowWriter w (Reader r c) where
  tellA = lift tellA

instance ArrowFail e c => ArrowFail e (Reader r c) where
  failA = lift failA

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
  fixA f = Reader (fixA (runReader . f . Reader))

instance ArrowExcept (r,x) y e c => ArrowExcept x y e (Reader r c) where
  tryCatchA (Reader f) (Reader g) = Reader $ tryCatchA f (from assoc ^>> g)
  finally (Reader f) (Reader g) = Reader $ finally f g

instance ArrowDeduplicate c => ArrowDeduplicate (Reader r c) where
  dedupA (Reader f) = Reader (dedupA f)

instance ArrowJoin c => ArrowJoin (Reader r c) where
  joinWith lub (Reader f) (Reader g) = Reader $ (\(r,(x,y)) -> ((r,x),(r,y))) ^>> joinWith lub f g

deriving instance PreOrd (c (r,x) y) => PreOrd (Reader r c x y)
deriving instance LowerBounded (c (r,x) y) => LowerBounded (Reader r c x y)
deriving instance Complete (c (r,x) y) => Complete (Reader r c x y)
deriving instance CoComplete (c (r,x) y) => CoComplete (Reader r c x y)
deriving instance UpperBounded (c (r,x) y) => UpperBounded (Reader r c x y)
