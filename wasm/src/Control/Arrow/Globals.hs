{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Arrow.Globals where

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

class ArrowGlobals v c | c -> v where
  -- | Reads a global variable. Cannot fail due to validation.
  readGlobal :: c GlobalAddr v
  -- | Writes a global variable. Cannot fail due to validation.
  writeGlobal :: c (GlobalAddr, v) ()

deriving instance (ArrowGlobals v c) => ArrowGlobals v (ValueT val2 c)
deriving instance (Arrow c, Profunctor c, ArrowGlobals v c) => ArrowGlobals v (CE.ExceptT e c)
deriving instance (O.Complete e, Arrow c, Profunctor c, ArrowGlobals v c) => ArrowGlobals v (AE.ExceptT e c)
instance (Arrow c, Profunctor c, Monad f, ArrowGlobals v c) => ArrowGlobals v (KleisliT f c) where
    readGlobal = lift' readGlobal
    writeGlobal = lift' writeGlobal
instance (Arrow c, Profunctor c, ArrowGlobals v c) => ArrowGlobals v (StateT s c) where
    readGlobal = lift' readGlobal
    writeGlobal = lift' writeGlobal
instance (Arrow c, Profunctor c, ArrowGlobals v c) => ArrowGlobals v (ReaderT r c) where
    readGlobal = lift' readGlobal
    writeGlobal = lift' writeGlobal
instance (Arrow c, Profunctor c, Monoid r, ArrowGlobals v c) => ArrowGlobals v (WriterT r c) where
    readGlobal = lift' readGlobal
    writeGlobal = lift' writeGlobal
