{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Arrow.Logger where

import           Prelude hiding (log)

import           Control.Arrow
import           Control.Arrow.Trans

import           Control.Arrow.Transformer.Concrete.Except
import           Control.Arrow.Transformer.Concrete.Frame
import           Control.Arrow.Transformer.Kleisli
import           Control.Arrow.Transformer.Reader
import           Control.Arrow.Transformer.Stack
import           Control.Arrow.Transformer.State
import           Control.Arrow.Transformer.Value

import           Control.Arrow.Transformer.Concrete.Failure

import Data.Profunctor

class ArrowLogger v c | c -> v where
    log :: c v ()


deriving instance (ArrowLogger v c) => ArrowLogger v (ValueT val c)
deriving instance (Arrow c, Profunctor c, ArrowLogger v c) => ArrowLogger v (FailureT e c)
instance (Monad f, Arrow c, Profunctor c, ArrowLogger v c) => ArrowLogger v (KleisliT f c) where
    log = lift' log

deriving instance (Arrow c, Profunctor c, ArrowLogger v c) => ArrowLogger v (FrameT fd val c)

instance (Arrow c, Profunctor c, ArrowLogger v c) => ArrowLogger v (ReaderT r c) where
    log = lift' log

instance (Arrow c, Profunctor c, ArrowLogger v c) => ArrowLogger v (StateT s c) where
    log = lift' log

deriving instance (Arrow c, Profunctor c, ArrowLogger v c) => ArrowLogger v (ExceptT e c)
deriving instance (Arrow c, Profunctor c, ArrowLogger v c) => ArrowLogger v (StackT s c)
