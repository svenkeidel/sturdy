{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Arrow.JumpTypes where

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
import           Language.Wasm.Structure (ResultType)

class ArrowJumpTypes c where
    jumpType :: c JumpIndex ResultType
    withJumpType :: c x y -> c (ResultType, x) y
    localNoJumpTypes :: c x y -> c x y


deriving instance (ArrowJumpTypes c) => ArrowJumpTypes (ValueT val2 c)
deriving instance (Arrow c, Profunctor c, ArrowJumpTypes c) => ArrowJumpTypes (CE.ExceptT e c)
deriving instance (O.Complete e, Arrow c, Profunctor c, ArrowJumpTypes c) => ArrowJumpTypes (AE.ExceptT e c)
instance (Arrow c, Profunctor c, Monad f, ArrowJumpTypes c) => ArrowJumpTypes (KleisliT f c) where
    jumpType = lift' jumpType
    withJumpType a = lift (withJumpType (unlift a))
    localNoJumpTypes a = lift (localNoJumpTypes (unlift a))
instance (Arrow c, Profunctor c, ArrowJumpTypes c) => ArrowJumpTypes (StateT s c) where
    jumpType = lift' jumpType
    withJumpType a = lift $ proc (s, (rt,x)) ->
        withJumpType (unlift a) -< (rt,(s,x))
    localNoJumpTypes a = lift (localNoJumpTypes (unlift a))
instance (Arrow c, Profunctor c, ArrowJumpTypes c) => ArrowJumpTypes (ReaderT r c) where
    jumpType = lift' jumpType
    withJumpType a = lift $ proc (r, (rt,x)) ->
        withJumpType (unlift a) -< (rt,(r,x))
    localNoJumpTypes a = lift (localNoJumpTypes (unlift a))
instance (Arrow c, Profunctor c, Monoid r, ArrowJumpTypes c) => ArrowJumpTypes (WriterT r c) where
    jumpType = lift' jumpType
    withJumpType a = lift (withJumpType (unlift a))
    localNoJumpTypes a = lift (localNoJumpTypes (unlift a))
