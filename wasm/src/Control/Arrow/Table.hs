{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Arrow.Table where

import           Control.Arrow
import           Control.Arrow.Trans

import           Control.Arrow.Transformer.Abstract.Except as AE
import           Control.Arrow.Transformer.Concrete.Except as CE
import           Control.Arrow.Transformer.Kleisli
import           Control.Arrow.Transformer.Reader
import           Control.Arrow.Transformer.State
import           Control.Arrow.Transformer.Value
import           Control.Arrow.Transformer.Writer

import           Data.Profunctor

class ArrowTable v c | c -> v where
    -- | readTable f g h (ta,ix,x)
    -- | Lookup `ix` in table `ta` to retrieve the function address `fa`.
    -- | Invokes `f (fa, x)` if all goes well.
    -- | Invokes `g (ta,ix,x)` if `ix` is out of bounds.
    -- | Invokes `h (ta,ix,x)` if `ix` cell is uninitialized.
    readTable :: c (Int,x) y -> c (Int,v,x) y -> c (Int,v,x) y -> c (Int,v,x) y

deriving instance (ArrowTable v c) => ArrowTable v (ValueT val2 c)
deriving instance (Arrow c, Profunctor c, ArrowTable v c) => ArrowTable v (CE.ExceptT e c)
deriving instance (Arrow c, Profunctor c, ArrowTable v c) => ArrowTable v (AE.ExceptT e c)
instance (Arrow c, Profunctor c, Functor f, ArrowTable v c) => ArrowTable v (KleisliT f c) where
    -- TODO
instance (Arrow c, ArrowTable v c) => ArrowTable v (StateT s c) where
    -- TODO
instance (Arrow c, ArrowTable v c) => ArrowTable v (ReaderT r c) where
    -- TODO
instance (ArrowTable v c) => ArrowTable v (WriterT r c) where
    -- TODO
