{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Derivations where
import           Data.Order
import           Syntax
instance PreOrd Location where
  (⊑) (Location a) (Location b) = a == b
  (≈) (Location a) (Location b) = a == b
instance Complete Location where
  (⊔) (Location a) (Location b) = case a < b of
    True  -> (Location b)
    False -> (Location a)
deriving instance Complete String
