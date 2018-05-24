{-# LANGUAGE RecordWildCards #-}
module Data.CallString where

import qualified Data.Foldable as F
import           Data.Label
import           Data.Hashable
import           Data.Sequence (Seq,(|>))
import qualified Data.Sequence as S

data CallString = CallString {
  callString :: Seq Label,
  size :: Int,
  maxSize :: Int
}

instance Show CallString where
  show = show . toList

instance Eq CallString where
  c1 == c2 = callString c1 == callString c2

instance Hashable CallString where
  hashWithSalt s = hashWithSalt s . F.toList . callString

empty :: Int -> CallString
empty m = CallString S.empty 0 m

push :: Label -> CallString -> CallString
push l (CallString {..}) = resize (CallString (callString |> l) (size + 1) maxSize)

resize :: CallString -> CallString
resize cont@(CallString {..})
  | size > maxSize = CallString (S.drop (size - maxSize) callString) maxSize maxSize
  | otherwise = cont

toList :: CallString -> [Label]
toList = F.toList . callString
