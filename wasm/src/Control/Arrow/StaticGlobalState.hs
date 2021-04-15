{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Arrow.StaticGlobalState where

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

class ArrowStaticGlobalState v c | c -> v where
  -- | Reads a global variable. Cannot fail due to validation.
  readGlobal :: c Int v
  -- | Writes a global variable. Cannot fail due to validation.
  writeGlobal :: c (Int, v) ()

  -- | Reads a function. Cannot fail due to validation.
  --readFunction :: c ((FuncType, ModuleInstance, Function), x) y -> c ((FuncType, HostFunction v c), x) y -> c (Int, x) y
  readFunction :: c ((FuncType, ModuleInstance, Function), x) y -> c (Int, x) y


deriving instance (ArrowStaticGlobalState v c) => ArrowStaticGlobalState v (ValueT val2 c)
deriving instance (Arrow c, Profunctor c, ArrowStaticGlobalState v c) => ArrowStaticGlobalState v (CE.ExceptT e c)
deriving instance (O.Complete e, Arrow c, Profunctor c, ArrowStaticGlobalState v c) => ArrowStaticGlobalState v (AE.ExceptT e c)
instance (Arrow c, Profunctor c, Monad f, ArrowStaticGlobalState v c) => ArrowStaticGlobalState v (KleisliT f c) where
    readGlobal = lift' readGlobal
    writeGlobal = lift' writeGlobal
    readFunction a = lift (readFunction (unlift a))
instance (Arrow c, Profunctor c, ArrowStaticGlobalState v c) => ArrowStaticGlobalState v (StateT s c) where
    readGlobal = lift' readGlobal
    writeGlobal = lift' writeGlobal
    readFunction a = lift $ transform (unlift a)
        where transform f = proc (s, (i,x)) -> readFunction (proc (y,(s2,x)) -> f -< (s2, (y,x))) -< (i,(s,x))
instance (Arrow c, Profunctor c, ArrowStaticGlobalState v c) => ArrowStaticGlobalState v (ReaderT r c) where
    readGlobal = lift' readGlobal
    writeGlobal = lift' writeGlobal
    readFunction a = lift $ transform (unlift a)
        where transform f = proc (r, (i,x)) ->
                                readFunction (proc (y,(r,x)) -> f -< (r, (y,x))) -< (i,(r,x))
instance (Arrow c, Profunctor c, Monoid r, ArrowStaticGlobalState v c) => ArrowStaticGlobalState v (WriterT r c) where
    readGlobal = lift' readGlobal
    writeGlobal = lift' writeGlobal
    readFunction a = lift (readFunction (unlift a))
