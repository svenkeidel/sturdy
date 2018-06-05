{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Derivations where
import Syntax
import Data.Order
deriving instance PreOrd Location
deriving instance Complete Location
deriving instance Complete String
