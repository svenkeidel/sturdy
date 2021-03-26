{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Control.Arrow.MemAddress where

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

class ArrowMemAddress base off addr c where
  memaddr :: c (base, off) addr

deriving instance (ArrowMemAddress base off addr c) => ArrowMemAddress base off addr (ValueT val2 c)
deriving instance (Arrow c, Profunctor c, ArrowMemAddress base off addr c) => ArrowMemAddress base off addr (CE.ExceptT e c)
deriving instance (O.Complete e, Arrow c, Profunctor c, ArrowMemAddress base off addr c) => ArrowMemAddress base off addr (AE.ExceptT e c)
instance (Monad f, Arrow c, Profunctor c, ArrowMemAddress base off addr c) => ArrowMemAddress base off addr (KleisliT f c) where
    memaddr = lift' memaddr
--deriving instance (Arrow c, Profunctor c, ArrowMemAddress base off addr c) => ArrowMemAddress base off addr (StackT v c)
instance (Arrow c, Profunctor c, ArrowMemAddress base off addr c) => ArrowMemAddress base off addr (StateT s c) where
    memaddr = lift' memaddr
instance (Arrow c, Profunctor c, ArrowMemAddress base off addr c) => ArrowMemAddress base off addr (ReaderT r c) where
    memaddr = lift' memaddr
instance (Monoid w, Arrow c, Profunctor c, ArrowMemAddress base off addr c) => ArrowMemAddress base off addr (WriterT w c) where
    memaddr = lift' memaddr
