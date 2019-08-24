{-# LANGUAGE FunctionalDependencies #-}
module Data.Abstract.Context where

import Data.Identifiable
import Data.Empty

class (IsEmpty ctx, Identifiable ctx) => IsContext ctx lab | ctx -> lab where
  push :: lab -> ctx -> ctx

