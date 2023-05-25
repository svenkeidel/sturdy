{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Control.Arrow.EffectiveAddress where

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

class ArrowEffectiveAddress base off addr c where
  effectiveAddress :: c (base, off) addr

deriving instance (ArrowEffectiveAddress base off addr c) => ArrowEffectiveAddress base off addr (ValueT val2 c)
deriving instance (Arrow c, Profunctor c, ArrowEffectiveAddress base off addr c) => ArrowEffectiveAddress base off addr (CE.ExceptT e c)
deriving instance (O.Complete e, Arrow c, Profunctor c, ArrowEffectiveAddress base off addr c) => ArrowEffectiveAddress base off addr (AE.ExceptT e c)
instance (Monad f, Arrow c, Profunctor c, ArrowEffectiveAddress base off addr c) => ArrowEffectiveAddress base off addr (KleisliT f c) where
    effectiveAddress = lift' effectiveAddress
--deriving instance (Arrow c, Profunctor c, ArrowEffectiveAddress base off addr c) => ArrowEffectiveAddress base off addr (StackT v c)
instance (Arrow c, Profunctor c, ArrowEffectiveAddress base off addr c) => ArrowEffectiveAddress base off addr (StateT s c) where
    effectiveAddress = lift' effectiveAddress
instance (Arrow c, Profunctor c, ArrowEffectiveAddress base off addr c) => ArrowEffectiveAddress base off addr (ReaderT r c) where
    effectiveAddress = lift' effectiveAddress
instance (Monoid w, Arrow c, Profunctor c, ArrowEffectiveAddress base off addr c) => ArrowEffectiveAddress base off addr (WriterT w c) where
    effectiveAddress = lift' effectiveAddress
