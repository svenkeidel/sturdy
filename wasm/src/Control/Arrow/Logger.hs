{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Arrow.Logger where

import           Prelude hiding (log)

import           Control.Arrow
import           Control.Arrow.Trans

import qualified Control.Arrow.Transformer.Abstract.Store as AbsStore
import           Control.Arrow.Transformer.Abstract.Error
import           Control.Arrow.Transformer.Concrete.Except as CE
import           Control.Arrow.Transformer.Abstract.Except as AE
import           Control.Arrow.Transformer.Concrete.Failure as CF
import           Control.Arrow.Transformer.Abstract.Failure as AF
import           Control.Arrow.Transformer.Concrete.WasmFrame as CFrame
import           Control.Arrow.Transformer.Abstract.WasmFrame as AFrame
import           Control.Arrow.Transformer.Kleisli
import           Control.Arrow.Transformer.Reader
import           Control.Arrow.Transformer.Stack
import           Control.Arrow.Transformer.State
import           Control.Arrow.Transformer.Value


import qualified Data.Order as O
import           Data.Profunctor

class ArrowLogger v c | c -> v where
    log :: c v ()


deriving instance (ArrowLogger v c) => ArrowLogger v (ValueT val c)
deriving instance (Arrow c, Profunctor c, ArrowLogger v c) => ArrowLogger v (CF.FailureT e c)
deriving instance (Arrow c, Profunctor c, ArrowLogger v c) => ArrowLogger v (AF.FailureT e c)
instance (Monad f, Arrow c, Profunctor c, ArrowLogger v c) => ArrowLogger v (KleisliT f c) where
    log = lift' log

deriving instance (Arrow c, Profunctor c, ArrowLogger v c) => ArrowLogger v (CFrame.FrameT fd val c)
deriving instance (Arrow c, Profunctor c, ArrowLogger v c) => ArrowLogger v (AFrame.FrameT fd val c)

instance (Arrow c, Profunctor c, ArrowLogger v c) => ArrowLogger v (ReaderT r c) where
    log = lift' log

instance (Arrow c, Profunctor c, ArrowLogger v c) => ArrowLogger v (StateT s c) where
    log = lift' log

deriving instance (Arrow c, Profunctor c, ArrowLogger v c) => ArrowLogger v (CE.ExceptT e c)
deriving instance (O.Complete e, Arrow c, Profunctor c, ArrowLogger v c) => ArrowLogger v (AE.ExceptT e c)
deriving instance (Arrow c, Profunctor c, ArrowLogger v c) => ArrowLogger v (ErrorT e c)
deriving instance (Arrow c, Profunctor c, ArrowLogger v c) => ArrowLogger v (StackT s c)
deriving instance (Arrow c, Profunctor c, ArrowLogger v c) => ArrowLogger v (AbsStore.StoreT store c)
