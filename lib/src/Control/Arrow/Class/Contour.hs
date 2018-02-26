{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
module Control.Arrow.Class.Contour(ArrowContour(..),Contour,empty,push,toList,size,maxSize) where

import           Data.Sequence (Seq,(|>))
import qualified Data.Sequence as S
import qualified Data.Foldable as F

data Contour l = Contour {
  contour :: (Seq l),
  size :: Int,
  maxSize :: Int
}

empty :: Int -> Contour l
empty m = Contour S.empty 0 m

push :: l -> Contour l -> Contour l
push l (Contour {..}) = resize (Contour (contour |> l) (size + 1) maxSize)

resize :: Contour l -> Contour l
resize cont@(Contour {..})
  | size > maxSize = Contour (S.drop (size - maxSize) contour) maxSize maxSize
  | otherwise = cont

toList :: Contour l -> [l]
toList = F.toList . contour

class ArrowContour l c | c -> l where
  askContour :: c () (Contour l)
