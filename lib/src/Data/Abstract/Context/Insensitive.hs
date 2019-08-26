{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Abstract.Context.Insensitive where

import Data.Empty
import Data.Hashable
import Data.Abstract.Context
import GHC.Generics

data ContextInsensitive lab = ContextInsensitive
  deriving stock (Show,Eq,Generic)
  deriving anyclass (Hashable)

instance IsEmpty (ContextInsensitive lab) where
  empty = ContextInsensitive
  {-# INLINE empty #-}

instance IsContext (ContextInsensitive lab) lab where
  push _ ContextInsensitive = ContextInsensitive
  {-# INLINE push #-}
