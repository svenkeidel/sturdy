{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Arrows #-}
module Control.Arrow.Transformer.Contour where

import           Prelude hiding (id,(.),lookup)

import           Control.Arrow
import           Control.Arrow.Class.Contour
import           Control.Arrow.Class.Reader
import           Control.Arrow.Class.State
import           Control.Arrow.Class.Fail
import           Control.Arrow.Class.Environment
import           Control.Arrow.Class.Fix
import           Control.Arrow.Transformer.Reader

import           Control.Category

newtype ContourArrow l c a b = ContourArrow (ReaderArrow (Contour l) c a b)

liftContour :: Arrow c => c a b -> ContourArrow l c a b
liftContour f = ContourArrow (liftReader f)

runContourArrow :: Arrow c => ContourArrow l c a b -> c (Int,a) b
runContourArrow (ContourArrow (ReaderArrow f)) = first empty ^>> f

instance Arrow c => Category (ContourArrow l c) where
  id = liftContour id
  ContourArrow f . ContourArrow g = ContourArrow (f . g)

instance Arrow c => Arrow (ContourArrow l c) where
  arr f = liftContour (arr f)
  first (ContourArrow f) = ContourArrow (first f)
  second (ContourArrow f) = ContourArrow (second f)

instance ArrowChoice c => ArrowChoice (ContourArrow l c) where
  left (ContourArrow f) = ContourArrow (left f)
  right (ContourArrow f) = ContourArrow (right f)

instance ArrowApply c => ArrowApply (ContourArrow l c) where
  app = ContourArrow $ (\(ContourArrow f,x) -> (f,x)) ^>> app

instance ArrowReader r c => ArrowReader r (ContourArrow l c) where
  askA = liftContour askA
  localA (ContourArrow (ReaderArrow f)) = ContourArrow $ ReaderArrow $ (\(cont,(r,x)) -> (r,(cont,x))) ^>> localA f

instance ArrowState s c => ArrowState s (ContourArrow l c) where
  getA = liftContour getA
  putA = liftContour putA

instance ArrowFail e c => ArrowFail e (ContourArrow l c) where
  failA = liftContour failA

instance ArrowEnv x y env c => ArrowEnv x y env (ContourArrow l c) where
  lookup = liftContour lookup
  getEnv = liftContour getEnv
  extendEnv = liftContour extendEnv
  localEnv (ContourArrow (ReaderArrow f)) = ContourArrow $ ReaderArrow $ (\(cont,(r,x)) -> (r,(cont,x))) ^>> localEnv f

instance (ArrowFix x y c, ArrowApply c) => ArrowFix x y (ContourArrow x c) where
  -- Pushes the last argument on the contour.
  fixA f = ContourArrow $ ReaderArrow $ proc (cont,x) -> fixA (unlift . f . lift (push x cont)) -<< x
    where
      lift :: Arrow c => Contour x -> c x y -> ContourArrow x c x y
      lift c f' = proc x -> localContour (liftContour f') -< (c,x)

      localContour :: Arrow c => ContourArrow l c x y -> ContourArrow l c (Contour l, x) y
      localContour (ContourArrow (ReaderArrow f')) = ContourArrow $ ReaderArrow $ snd ^>> f'

      unlift :: Arrow c => ContourArrow x c x y -> c x y
      unlift f' = (const 0 &&& id) ^>> runContourArrow f'
