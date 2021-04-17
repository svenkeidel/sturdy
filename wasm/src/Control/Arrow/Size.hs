{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Arrow.Size where

import           Control.Arrow
import           Control.Arrow.Trans

import           Control.Arrow.Transformer.Abstract.Except as AE
import           Control.Arrow.Transformer.Concrete.Except as CE
import           Control.Arrow.Transformer.Kleisli
import           Control.Arrow.Transformer.Reader
import           Control.Arrow.Transformer.State
import           Control.Arrow.Transformer.Value
import           Control.Arrow.Transformer.Writer

import qualified Data.Order as O
import           Data.Profunctor

class ArrowSize v sz c | c -> v, c -> sz where
    valToSize :: c v sz
    sizeToVal :: c sz v

deriving instance (ArrowSize v sz c) => ArrowSize v sz (ValueT v2 c)
deriving instance (Arrow c, Profunctor c, ArrowSize v sz c) => ArrowSize v sz (CE.ExceptT e c)
deriving instance (O.Complete e, Arrow c, Profunctor c, ArrowSize v sz c) => ArrowSize v sz (AE.ExceptT e c)
instance (Arrow c, Profunctor c, Monad f, ArrowSize v sz c) => ArrowSize v sz (KleisliT f c) where
    valToSize = lift' valToSize
    sizeToVal = lift' sizeToVal
instance (Arrow c, Profunctor c, ArrowSize v sz c) => ArrowSize v sz (StateT s c) where
    valToSize = lift' valToSize
    sizeToVal = lift' sizeToVal
instance (Arrow c, Profunctor c, ArrowSize v sz c) => ArrowSize v sz (ReaderT s c) where
    valToSize = lift' valToSize
    sizeToVal = lift' sizeToVal
instance (Arrow c, Profunctor c, Monoid s, ArrowSize v sz c) => ArrowSize v sz (WriterT s c) where
    valToSize = lift' valToSize
    sizeToVal = lift' sizeToVal
