{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Control.Arrow.MemSizable where

import           Control.Arrow
import           Control.Arrow.Trans

import           Control.Arrow.Transformer.Concrete.Except as CE
import           Control.Arrow.Transformer.Abstract.Except as AE
import           Control.Arrow.Transformer.Kleisli
import           Control.Arrow.Transformer.Reader
--import           Control.Arrow.Transformer.Stack
import           Control.Arrow.Transformer.State
import           Control.Arrow.Transformer.Value
import           Control.Arrow.Transformer.Writer

import qualified Data.Order as O
import           Data.Profunctor

class ArrowMemSizable sz c where
  memsize :: c Int sz
  memgrow :: c (sz,x) y -> c x y -> c (Int,sz,x) y

deriving instance (ArrowMemSizable sz c) => ArrowMemSizable sz (ValueT val2 c)
deriving instance (Arrow c, Profunctor c, ArrowMemSizable sz c) => ArrowMemSizable sz (CE.ExceptT e c)
deriving instance (O.Complete e, Arrow c, Profunctor c, ArrowMemSizable sz c) => ArrowMemSizable sz (AE.ExceptT e c)
instance (Monad f, Arrow c, Profunctor c, ArrowMemSizable sz c) => ArrowMemSizable sz (KleisliT f c) where
    memsize = lift' memsize
    memgrow = error "TODO: implement KleisliT.memgrow"
--deriving instance (ArrowMemSizable sz c) => ArrowMemSizable sz (StackT v c)
instance (ArrowMemSizable sz c) => ArrowMemSizable sz (StateT s c) where
    memsize = error "TODO: implement StateT.memSize"
    memgrow = error "TODO: implement StateT.memgrow"
instance (ArrowMemSizable sz c) => ArrowMemSizable sz (ReaderT r c) where
    memsize = error "TODO: implement ReaderT.memSize"
    memgrow = error "TODO: implement ReaderT.memgrow"
instance (ArrowMemSizable sz c) => ArrowMemSizable sz (WriterT r c) where
    memsize = error "TODO: implement WriterT.memSize"
    memgrow = error "TODO: implement WriterT.memgrow"
