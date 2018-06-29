{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Derivations where
import           Data.Order
import           Syntax
deriving instance PreOrd Location
deriving instance Complete Location
deriving instance Complete String
