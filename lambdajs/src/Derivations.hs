{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
module Derivations where
import Syntax
import Data.Order
deriving instance PreOrd Location
deriving instance Complete Location