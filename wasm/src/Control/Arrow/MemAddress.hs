{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Control.Arrow.MemAddress where

import           Control.Arrow.Transformer.Concrete.Except
import           Control.Arrow.Transformer.Kleisli
import           Control.Arrow.Transformer.Reader
import           Control.Arrow.Transformer.Stack
import           Control.Arrow.Transformer.State
import           Control.Arrow.Transformer.Value


class ArrowMemAddress base off addr c where
  memaddr :: c (base, off) addr

deriving instance (ArrowMemAddress base off addr c) => ArrowMemAddress base off addr (ValueT val2 c)
deriving instance (ArrowMemAddress base off addr c) => ArrowMemAddress base off addr (ExceptT e c)
instance (ArrowMemAddress base off addr c) => ArrowMemAddress base off addr (KleisliT f c) where
    -- TODO
deriving instance (ArrowMemAddress base off addr c) => ArrowMemAddress base off addr (StackT v c)
instance (ArrowMemAddress base off addr c) => ArrowMemAddress base off addr (StateT s c) where
    -- TODO
instance (ArrowMemAddress base off addr c) => ArrowMemAddress base off addr (ReaderT r c) where
    -- TODO
