{-# LANGUAGE Arrows #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.DebuggableStack where

import           Control.Arrow
import           Control.Arrow.Stack (ArrowStack)
import           Control.Arrow.Trans
import           Control.Arrow.Transformer.Kleisli
import           Control.Arrow.Transformer.Reader
import           Control.Arrow.Transformer.State
import           Control.Arrow.Transformer.Value
import           Control.Arrow.Transformer.Writer

import           Data.Profunctor.Unsafe


class ArrowStack v c => ArrowDebuggableStack v c | c -> v where
    getStack :: c () [v]


---------------- instances -------------------------

instance (Profunctor c, Arrow c, Monad f, ArrowDebuggableStack v c) => ArrowDebuggableStack v (KleisliT f c) where
    getStack = lift' getStack

instance (Profunctor c, Arrow c, ArrowDebuggableStack v c) => ArrowDebuggableStack v (ReaderT r c) where
    getStack = lift' getStack

instance (Profunctor c, Arrow c, ArrowDebuggableStack v c) => ArrowDebuggableStack v (StateT st c) where
    getStack = lift' getStack

deriving instance (ArrowDebuggableStack v c) => ArrowDebuggableStack v (ValueT val c)

instance (Monoid w, Profunctor c, Arrow c, ArrowDebuggableStack v c) => ArrowDebuggableStack v (WriterT w c) where
    getStack = lift' getStack
