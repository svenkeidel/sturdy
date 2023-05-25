{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Arrow.Functions where

import           Data

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
import           Language.Wasm.Structure (FuncType)
import           Language.Wasm.Interpreter (ModuleInstance(..))

class ArrowFunctions c where
  -- | Reads a function. Cannot fail due to validation.
  --readFunction :: c ((FuncType, ModuleInstance, Function), x) y -> c ((FuncType, HostFunction v c), x) y -> c (Int, x) y
  --readFunction :: c ((FuncType, ModuleInstance, Function), x) y -> c (Int, x) y
  readFunction :: c FunctionAddr (FuncType,ModuleInstance,Function)


deriving instance (ArrowFunctions c) => ArrowFunctions (ValueT val2 c)
deriving instance (Arrow c, Profunctor c, ArrowFunctions c) => ArrowFunctions (CE.ExceptT e c)
deriving instance (O.Complete e, Arrow c, Profunctor c, ArrowFunctions c) => ArrowFunctions (AE.ExceptT e c)
instance (Arrow c, Profunctor c, Monad f, ArrowFunctions c) => ArrowFunctions (KleisliT f c) where
    readFunction = lift' readFunction
instance (Arrow c, Profunctor c, ArrowFunctions c) => ArrowFunctions (StateT s c) where
    readFunction = lift' readFunction
instance (Arrow c, Profunctor c, ArrowFunctions c) => ArrowFunctions (ReaderT r c) where
    readFunction = lift' readFunction
instance (Arrow c, Profunctor c, Monoid r, ArrowFunctions c) => ArrowFunctions (WriterT r c) where
    readFunction = lift' readFunction
