{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Arrow.Memory where

import           Control.Arrow.Transformer.Concrete.Except
import           Control.Arrow.Transformer.Kleisli
import           Control.Arrow.Transformer.Reader
import           Control.Arrow.Transformer.Stack
import           Control.Arrow.Transformer.State
import           Control.Arrow.Transformer.Value

class ArrowMemory addr bytes c | c -> addr, c -> bytes where
  memread :: c (bytes, x) y -> c x y -> c (addr, Int, x) y
  memstore :: c x y -> c x y -> c (addr, bytes, x) y

deriving instance (ArrowMemory addr bytes c) => ArrowMemory addr bytes (ValueT val2 c)
deriving instance (ArrowMemory addr bytes c) => ArrowMemory addr bytes (ExceptT e c)
instance (ArrowMemory addr bytes c) => ArrowMemory addr bytes (KleisliT e c) where
    -- TODO
deriving instance (ArrowMemory addr bytes c) => ArrowMemory addr bytes (StackT e c)
instance (ArrowMemory addr bytes c) => ArrowMemory addr bytes (StateT s c) where
    -- TODO
instance (ArrowMemory addr bytes c) => ArrowMemory addr bytes (ReaderT r c) where
    -- TODO
