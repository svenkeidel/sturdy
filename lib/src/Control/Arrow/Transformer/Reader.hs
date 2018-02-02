{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Transformer.Reader where

import Prelude hiding (id,(.))
import Control.Category
import Control.Arrow
import Control.Arrow.Class.Fail
import Control.Arrow.Class.Reader
import Control.Arrow.Class.State
import Control.Arrow.Utils

newtype ReaderArrow r c x y = ReaderArrow { runReaderArrow :: c (r,x) y }

liftReader :: Arrow c => c x y -> ReaderArrow r c x y
liftReader f = ReaderArrow (pi2 >>> f)

instance Arrow c => Category (ReaderArrow r c) where
  id = liftReader id
  ReaderArrow f . ReaderArrow g = ReaderArrow $ (\(r,x) -> (r,(r,x))) ^>> f . second g

instance Arrow c => Arrow (ReaderArrow r c) where
  arr f = liftReader (arr f)
  first (ReaderArrow f) = ReaderArrow $ (\(r,(x,y)) -> ((r,x),y)) ^>> first f
  second (ReaderArrow f) = ReaderArrow $ (\(r,(x,y)) -> (x,(r,y))) ^>> second f

instance ArrowChoice c => ArrowChoice (ReaderArrow r c) where
  left (ReaderArrow f) = ReaderArrow $ injectLeft ^>> left f
  right (ReaderArrow f) = ReaderArrow $ injectRight ^>> right f

instance Arrow c => ArrowReader r (ReaderArrow r c) where
  askA = ReaderArrow pi1
  localA (ReaderArrow f) = ReaderArrow (pi2 >>> f)

instance ArrowState s c => ArrowState s (ReaderArrow r c) where
  getA = liftReader getA
  putA = liftReader putA

instance ArrowFail e c => ArrowFail e (ReaderArrow r c) where
  failA = liftReader failA
