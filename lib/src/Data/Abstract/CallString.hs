{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Abstract.CallString(CallString,push,truncate) where

import           Prelude hiding (truncate)

import qualified Data.Foldable as F
import           Data.Hashable
import           Data.Sequence (Seq,(|>))
import qualified Data.Sequence as S
import           Data.Empty

data CallString lab = CallString { callString :: Seq lab, size :: Int }

instance Show lab => Show (CallString lab) where
  show = show . toList

instance Eq lab => Eq (CallString lab) where
  c1 == c2 = size c1 == size c2 && callString c1 == callString c2
  {-# INLINE (==) #-}

instance Hashable lab => Hashable (CallString lab) where
  hashWithSalt s = hashWithSalt s . toList
  {-# INLINE hashWithSalt #-}

instance IsEmpty (CallString lab) where
  empty = CallString S.empty 0
  {-# INLINE empty #-}

push :: lab -> CallString lab -> CallString lab
push l (CallString {..}) = CallString (callString |> l) (size + 1)
{-# INLINE push #-}

truncate :: Int -> CallString lab -> CallString lab
truncate k cont@(CallString {..})
  | size > k = CallString (S.drop (size - k) callString) k
  | otherwise = cont

toList :: CallString lab -> [lab]
toList = F.toList . callString
{-# INLINE toList #-}
