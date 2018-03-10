{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Transformer.Contour(Contour,empty,push,toList,size,maxSize,ContourArrow,runContourArrow,liftContour) where

import           Prelude hiding (id,(.),lookup)

import           Control.Arrow
import           Control.Arrow.Class.Alloc
import           Control.Arrow.Class.Reader
import           Control.Arrow.Class.State
import           Control.Arrow.Class.Fail
import           Control.Arrow.Class.Environment
import           Control.Arrow.Class.Fix

import           Control.Category

import qualified Data.Foldable as F
import           Data.Label
import           Data.Order
import           Data.Hashable
import           Data.Sequence (Seq,(|>))
import qualified Data.Sequence as S

data Contour l = Contour {
  contour :: Seq (Hashed l),
  size :: Int,
  maxSize :: Int
}

instance Show l => Show (Contour l) where
  show = show . toList

instance Eq l => Eq (Contour l) where
  c1 == c2 = contour c1 == contour c2

instance Hashable (Contour l) where
  hashWithSalt s = hashWithSalt s . F.toList . contour

empty :: Int -> Contour l
empty m = Contour S.empty 0 m

push :: Hashable l => l -> Contour l -> Contour l
push l (Contour {..}) = resize (Contour (contour |> hashed l) (size + 1) maxSize)

resize :: Contour l -> Contour l
resize cont@(Contour {..})
  | size > maxSize = Contour (S.drop (size - maxSize) contour) maxSize maxSize
  | otherwise = cont

toList :: Contour l -> [l]
toList = map unhashed . F.toList . contour

newtype ContourArrow l c a b = ContourArrow (Contour l -> c a b)

liftContour :: c a b -> ContourArrow l c a b
liftContour f = ContourArrow (const f)

runContourArrow :: Int -> ContourArrow l c a b -> c a b
runContourArrow n (ContourArrow f) = f (empty n)

instance Arrow c => Category (ContourArrow l c) where
  id = liftContour id
  ContourArrow f . ContourArrow g = ContourArrow $ \l -> (f l . g l)

instance Arrow c => Arrow (ContourArrow l c) where
  arr f = liftContour (arr f)
  first (ContourArrow f) = ContourArrow (first . f)
  second (ContourArrow f) = ContourArrow (second . f)

instance ArrowChoice c => ArrowChoice (ContourArrow l c) where
  left (ContourArrow f) = ContourArrow (left . f)
  right (ContourArrow f) = ContourArrow (right . f)

instance ArrowApply c => ArrowApply (ContourArrow l c) where
  app = ContourArrow $ \l -> (\(ContourArrow f,x) -> (f l,x)) ^>> app

instance ArrowReader r c => ArrowReader r (ContourArrow l c) where
  askA = liftContour askA
  localA (ContourArrow f) = ContourArrow $ \l -> (\(r,x) -> (r,x)) ^>> localA (f l)

instance ArrowState s c => ArrowState s (ContourArrow l c) where
  getA = liftContour getA
  putA = liftContour putA

instance ArrowFail e c => ArrowFail e (ContourArrow l c) where
  failA = liftContour failA

instance ArrowEnv x y env c => ArrowEnv x y env (ContourArrow l c) where
  lookup = liftContour lookup
  getEnv = liftContour getEnv
  extendEnv = liftContour extendEnv
  localEnv (ContourArrow f) = ContourArrow $ \l -> (\(r,x) -> (r,x)) ^>> localEnv (f l)

instance (ArrowFix x y c, ArrowApply c, Label x l, Hashable l) => ArrowFix x y (ContourArrow l c) where
  -- Pushes the label of the last argument on the contour.
  fixA f = ContourArrow $ \l -> proc x -> fixA (unlift l . f . lift) -<< x
    where
      lift :: c x y -> ContourArrow l c x y
      lift = liftContour

      unlift :: (Hashable l, Label x l, ArrowApply c) => Contour l -> ContourArrow l c x y -> c x y
      unlift cont (ContourArrow f') = proc x -> do
        y <- f' (push (getLabel x) cont) -<< x
        returnA -< y

instance Arrow c => ArrowAlloc var (var,Contour l) val (ContourArrow l c) where
  alloc = ContourArrow $ \l -> proc (x,_,_) -> returnA -< (x,l)

deriving instance PreOrd (c x y) => PreOrd (ContourArrow l c x y)
deriving instance LowerBounded (c x y) => LowerBounded (ContourArrow l c x y)
deriving instance Complete (c x y) => Complete (ContourArrow l c x y)
deriving instance CoComplete (c x y) => CoComplete (ContourArrow l c x y)
deriving instance UpperBounded (c x y) => UpperBounded (ContourArrow l c x y)
