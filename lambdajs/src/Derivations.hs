{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- TODO
-- This file was necessary due to some conflicting language feature pragmas.
-- This file might not be necessary anymore because some of the derivations in this file were removed, but I'm not sure at this time.

module Derivations where
-- import           Data.Order
-- import           Syntax
-- instance PreOrd Location where
--   (⊑) (Location a) (Location b) = a == b
--   (≈) (Location a) (Location b) = a == b
-- instance Complete Location where
--   (⊔) (Location a) (Location b) = case a < b of
--     True  -> (Location b)
--     False -> (Location a)
-- deriving instance Complete String
