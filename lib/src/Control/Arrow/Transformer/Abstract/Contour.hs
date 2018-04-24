{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Transformer.Abstract.Contour(Contour,empty,push,toList,size,maxSize,ContourArrow,runContourArrow) where

import           Prelude hiding (id,(.),lookup)

import           Control.Arrow
import           Control.Arrow.Abstract.Alloc
import           Control.Arrow.Environment
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.Lift
import           Control.Arrow.Reader
import           Control.Arrow.State
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

newtype ContourArrow c a b = ContourArrow (Reader Contour c a b)

runContourArrow :: Arrow c => Int -> ContourArrow c a b -> c a b
runContourArrow n (ContourArrow (Reader f)) = (\a -> (empty n,a)) ^>> f

type instance Fix x y (ContourArrow c) = ContourArrow (Fix x y c)
instance (ArrowFix x y c, ArrowApply c, HasLabel x) => ArrowFix x y (ContourArrow c) where
  -- Pushes the label of the last argument on the contour.
  fixA f = ContourArrow $ Reader $ proc (c,x) -> fixA (unlift c . f . lift) -<< x
    where
      unlift :: (HasLabel x, ArrowApply c) => Contour -> ContourArrow c x y -> c x y
      unlift c (ContourArrow (Reader f')) = proc x -> do
        y <- f' -< (push (label x) c, x)
        returnA -< y

instance Arrow c => ArrowAlloc var (var,Contour) val (ContourArrow c) where
  alloc = ContourArrow $ Reader $ proc (l,(x,_,_)) -> returnA -< (x,l)

instance ArrowApply c => ArrowApply (ContourArrow c) where
  app = ContourArrow $ (\(ContourArrow f,x) -> (f,x)) ^>> app

instance ArrowReader r c => ArrowReader r (ContourArrow c) where
  askA = lift askA
  localA (ContourArrow (Reader f)) = ContourArrow (Reader ((\(c,(r,x)) -> (r,(c,x))) ^>> localA f))

deriving instance Arrow c => Category (ContourArrow c)
deriving instance Arrow c => Arrow (ContourArrow c)
deriving instance ArrowLift ContourArrow
deriving instance ArrowChoice c => ArrowChoice (ContourArrow c)
deriving instance ArrowState s c => ArrowState s (ContourArrow c)
deriving instance ArrowFail e c => ArrowFail e (ContourArrow c)
deriving instance ArrowEnv x y env c => ArrowEnv x y env (ContourArrow c)

deriving instance PreOrd (c (Contour,x) y) => PreOrd (ContourArrow c x y)
deriving instance LowerBounded (c (Contour,x) y) => LowerBounded (ContourArrow c x y)
deriving instance Complete (c (Contour,x) y) => Complete (ContourArrow c x y)
deriving instance CoComplete (c (Contour,x) y) => CoComplete (ContourArrow c x y)
deriving instance UpperBounded (c (Contour,x) y) => UpperBounded (ContourArrow c x y)
