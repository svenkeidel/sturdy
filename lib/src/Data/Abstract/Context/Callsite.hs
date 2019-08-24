{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Abstract.Context.Callsite where

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

data CallsiteSensitive (maxSize :: Nat) lab = CallsiteSensitive { callString :: Seq lab, size :: Int }

instance Show lab => Show (CallsiteSensitive k lab) where
  show = show . toList

instance Eq lab => Eq (CallsiteSensitive k lab) where
  c1 == c2 = size c1 == size c2 && callString c1 == callString c2
  {-# INLINE (==) #-}

instance Hashable lab => Hashable (CallsiteSensitive k lab) where
  hashWithSalt s = hashWithSalt s . toList
  {-# INLINE hashWithSalt #-}

instance IsEmpty (CallsiteSensitive k lab) where
  empty = CallsiteSensitive S.empty 0
  {-# INLINE empty #-}

instance (Identifiable lab, KnownNat k) => IsContext (CallsiteSensitive k lab) lab where
  push l (CallsiteSensitive {..}) = truncate (CallsiteSensitive (callString |> l) (size + 1))
  {-# INLINE push #-}

truncate :: forall lab k. KnownNat k => CallsiteSensitive k lab -> CallsiteSensitive k lab
truncate cont@(CallsiteSensitive {..})
  | size > maxSize = CallsiteSensitive (S.drop (size - maxSize) callString) maxSize
  | otherwise = cont
  where
    maxSize = fromInteger (natVal @k Proxy)
{-# INLINE truncate #-}

toList :: CallsiteSensitive k lab -> [lab]
toList = F.toList . callString
{-# INLINE toList #-}
