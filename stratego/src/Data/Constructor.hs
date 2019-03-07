{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Constructor where
import Control.DeepSeq(NFData)

import Data.String(IsString(..))
import Data.Hashable(Hashable)
import Data.Text (Text,singleton,unpack)

import Test.QuickCheck

newtype Constructor = Constructor { toText :: Text } deriving (Eq,Ord,IsString,Hashable,NFData)

arbitraryLetter :: Gen Text
arbitraryLetter = singleton <$> choose ('A','Z')

instance Arbitrary Constructor where
  arbitrary = Constructor <$> arbitraryLetter

instance Show Constructor where
  show (Constructor c) = unpack c
