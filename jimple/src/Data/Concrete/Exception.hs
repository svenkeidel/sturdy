{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Concrete.Exception where

import Data.Hashable

data Exception v = StaticException String | DynamicException v deriving (Eq)

instance Show v => Show (Exception v) where
  show (StaticException s) = "Static: " ++ s
  show (DynamicException v) = "Dynamic: " ++ show v

instance Hashable v => Hashable (Exception v) where
  hashWithSalt h (StaticException s) = h + hash s
  hashWithSalt h (DynamicException v) = h + hash v
