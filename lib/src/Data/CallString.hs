{-# LANGUAGE RecordWildCards #-}
module Data.CallString where

import qualified Data.Foldable as F
import           Data.Hashable
import           Data.Sequence (Seq,(|>))
import qualified Data.Sequence as S

data CallString lab = CallString {
  callString :: Seq lab,
  size :: Int,
  maxSize :: Int
}

instance Show lab => Show (CallString lab) where
  show = show . toList

instance Eq lab => Eq (CallString lab) where
  c1 == c2 = callString c1 == callString c2

instance Hashable lab => Hashable (CallString lab) where
  hashWithSalt s = hashWithSalt s . F.toList . callString

empty :: Int -> CallString lab
empty m = CallString S.empty 0 m

push :: lab -> CallString lab -> CallString lab
push l (CallString {..}) = resize (CallString (callString |> l) (size + 1) maxSize)

resize :: CallString lab -> CallString lab
resize cont@(CallString {..})
  | size > maxSize = CallString (S.drop (size - maxSize) callString) maxSize maxSize
  | otherwise = cont

toList :: CallString lab -> [lab]
toList = F.toList . callString
