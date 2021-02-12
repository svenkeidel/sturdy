{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Control.Arrow.MemSizable where

import           Control.Arrow
import           Control.Arrow.Trans

import           Control.Arrow.Transformer.Concrete.Except
import           Control.Arrow.Transformer.Kleisli
import           Control.Arrow.Transformer.Reader
import           Control.Arrow.Transformer.Stack
import           Control.Arrow.Transformer.State
import           Control.Arrow.Transformer.Value

import           Data.Profunctor

class ArrowMemSizable sz c where
  memsize :: c () sz
  memgrow :: c (sz,x) y -> c x y -> c (sz,x) y

deriving instance (ArrowMemSizable sz c) => ArrowMemSizable sz (ValueT val2 c)
deriving instance (Arrow c, Profunctor c, ArrowMemSizable sz c) => ArrowMemSizable sz (ExceptT e c)
instance (Monad f, Arrow c, Profunctor c, ArrowMemSizable sz c) => ArrowMemSizable sz (KleisliT f c) where
    memsize = lift' memsize
    -- TODO
deriving instance (ArrowMemSizable sz c) => ArrowMemSizable sz (StackT v c)
instance (ArrowMemSizable sz c) => ArrowMemSizable sz (StateT s c) where
    -- TODO
instance (ArrowMemSizable sz c) => ArrowMemSizable sz (ReaderT r c) where
    -- TODO
