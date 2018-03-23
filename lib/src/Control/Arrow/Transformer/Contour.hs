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
import           Control.Arrow.Transformer.Reader

import           Control.Category

import qualified Data.Foldable as F
import           Data.Label
import           Data.Order
import           Data.Hashable
import           Data.Sequence (Seq,(|>))
import qualified Data.Sequence as S

data Contour = Contour {
  contour :: Seq Label,
  size :: Int,
  maxSize :: Int
}

instance Show Contour where
  show = show . toList

instance Eq Contour where
  c1 == c2 = contour c1 == contour c2

instance Hashable Contour where
  hashWithSalt s = hashWithSalt s . F.toList . contour

empty :: Int -> Contour
empty m = Contour S.empty 0 m

push :: Label -> Contour -> Contour
push l (Contour {..}) = resize (Contour (contour |> l) (size + 1) maxSize)

resize :: Contour -> Contour
resize cont@(Contour {..})
  | size > maxSize = Contour (S.drop (size - maxSize) contour) maxSize maxSize
  | otherwise = cont

toList :: Contour -> [Label]
toList = F.toList . contour

newtype ContourArrow c a b = ContourArrow (ReaderArrow Contour c a b)

liftContour :: Arrow c => c a b -> ContourArrow c a b
liftContour f = ContourArrow (liftReader f)

runContourArrow :: Arrow c => Int -> ContourArrow c a b -> c a b
runContourArrow n (ContourArrow (ReaderArrow f)) = (\a -> (empty n,a)) ^>> f

deriving instance Arrow c => Category (ContourArrow c)
deriving instance Arrow c => Arrow (ContourArrow c)
deriving instance ArrowChoice c => ArrowChoice (ContourArrow c)
deriving instance ArrowState s c => ArrowState s (ContourArrow c)
instance ArrowApply c => ArrowApply (ContourArrow c) where
  app = ContourArrow $ (\(ContourArrow f,x) -> (f,x)) ^>> app

instance ArrowReader r c => ArrowReader r (ContourArrow c) where
  askA = liftContour askA
  localA (ContourArrow (ReaderArrow f)) = ContourArrow (ReaderArrow ((\(c,(r,x)) -> (r,(c,x))) ^>> localA f))

deriving instance ArrowFail e c => ArrowFail e (ContourArrow c)
instance ArrowEnv x y env c => ArrowEnv x y env (ContourArrow c) where
  lookup = liftContour lookup
  getEnv = liftContour getEnv
  extendEnv = liftContour extendEnv
  localEnv (ContourArrow (ReaderArrow f)) = ContourArrow (ReaderArrow ((\(c,(r,x)) -> (r,(c,x))) ^>> localEnv f))

instance (ArrowFix x y c, ArrowApply c, HasLabel x) => ArrowFix x y (ContourArrow c) where
  -- Pushes the label of the last argument on the contour.
  fixA f = ContourArrow $ ReaderArrow $ proc (c,x) -> fixA (unlift c . f . lift) -<< x
    where
      lift :: Arrow c => c x y -> ContourArrow c x y
      lift = liftContour

      unlift :: (HasLabel x, ArrowApply c) => Contour -> ContourArrow c x y -> c x y
      unlift c (ContourArrow (ReaderArrow f')) = proc x -> do
        y <- f' -< (push (label x) c, x)
        returnA -< y

instance Arrow c => ArrowAlloc var (var,Contour) val (ContourArrow c) where
  alloc = ContourArrow $ ReaderArrow $ proc (l,(x,_,_)) -> returnA -< (x,l)

deriving instance PreOrd (c (Contour,x) y) => PreOrd (ContourArrow c x y)
deriving instance LowerBounded (c (Contour,x) y) => LowerBounded (ContourArrow c x y)
deriving instance Complete (c (Contour,x) y) => Complete (ContourArrow c x y)
deriving instance CoComplete (c (Contour,x) y) => CoComplete (ContourArrow c x y)
deriving instance UpperBounded (c (Contour,x) y) => UpperBounded (ContourArrow c x y)
