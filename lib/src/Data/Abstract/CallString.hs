{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Abstract.CallString where

import           Prelude hiding (truncate)

import qualified Data.Foldable as F
import           Data.Hashable
import           Data.Sequence (Seq,(|>))
import qualified Data.Sequence as S
import           Data.Empty
import           Data.Proxy
import           Data.Identifiable
import           Data.Abstract.Context
import           GHC.TypeLits

data CallString (maxSize :: Nat) lab = CallString { callString :: Seq lab, size :: Int }

instance Show lab => Show (CallString k lab) where
  show = show . toList

instance Eq lab => Eq (CallString k lab) where
  c1 == c2 = size c1 == size c2 && callString c1 == callString c2
  {-# INLINE (==) #-}

instance Hashable lab => Hashable (CallString k lab) where
  hashWithSalt s = hashWithSalt s . toList
  {-# INLINE hashWithSalt #-}

instance IsEmpty (CallString k lab) where
  empty = CallString S.empty 0
  {-# INLINE empty #-}

instance (Identifiable lab, KnownNat k) => IsContext (CallString k lab) lab where
  push l (CallString {..}) = truncate (CallString (callString |> l) (size + 1))
  {-# INLINE push #-}

truncate :: forall lab k. KnownNat k => CallString k lab -> CallString k lab
truncate cont@(CallString {..})
  | size > maxSize = CallString (S.drop (size - maxSize) callString) maxSize
  | otherwise = cont
  where
    maxSize = fromInteger (natVal @k Proxy)
{-# INLINE truncate #-}

toList :: CallString k lab -> [lab]
toList = F.toList . callString
{-# INLINE toList #-}
