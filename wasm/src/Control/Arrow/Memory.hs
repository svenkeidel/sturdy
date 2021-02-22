{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Arrow.Memory where

import           Control.Arrow

import           Control.Arrow.Transformer.Concrete.Except
import           Control.Arrow.Transformer.Kleisli
import           Control.Arrow.Transformer.Reader
import           Control.Arrow.Transformer.Stack
import           Control.Arrow.Transformer.State
import           Control.Arrow.Transformer.Value
import           Control.Arrow.Transformer.Writer

class ArrowMemory m addr bytes c | c -> addr, c -> bytes, c -> m where
  memread :: c (bytes, x) y -> c x y -> c (m, (addr, Int, x)) (m,y)
  memstore :: c x y -> c x y -> c (m, (addr, bytes, x)) (m,y)

--  getMemory :: c () m
--  putMemory :: c m ()
--
--withMemory :: (Arrow c, ArrowMemory m addr bytes c) => c x y -> c (m,x) (m,y)
--withMemory f = proc (m,x) -> do
--    putMemory -< m
--    y <- f -< x
--    newMem <- getMemory -< ()
--    returnA -< (m,y)

deriving instance (ArrowMemory m addr bytes c) => ArrowMemory m addr bytes (ValueT val2 c)
deriving instance (ArrowMemory m addr bytes c) => ArrowMemory m addr bytes (ExceptT e c)
instance (ArrowMemory m addr bytes c) => ArrowMemory m addr bytes (KleisliT e c) where
    -- TODO
deriving instance (ArrowMemory m addr bytes c) => ArrowMemory m addr bytes (StackT e c)
instance (ArrowMemory m addr bytes c) => ArrowMemory m addr bytes (StateT s c) where
    -- TODO
instance (ArrowMemory m addr bytes c) => ArrowMemory m addr bytes (ReaderT r c) where
    -- TODO
instance (ArrowMemory m addr bytes c) => ArrowMemory m addr bytes (WriterT r c) where
    -- TODO
