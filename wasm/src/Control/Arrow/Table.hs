{-# LANGUAGE Arrows #-}
{-# LANGUAGE ConstraintKinds #-}
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
import           Data.Kind(Type)

import           GHC.Exts

class ArrowTable v c | c -> v where
    type family JoinTable y (c :: Type -> Type -> Type) :: Constraint
    -- | readTable f g h (ta,ix,x)
    -- | Lookup `ix` in table `ta` to retrieve the function address `fa`.
    -- | Invokes `f (fa, x)` if all goes well.
    -- | Invokes `g (ta,ix,x)` if `ix` is out of bounds.
    -- | Invokes `h (ta,ix,x)` if `ix` cell is uninitialized.
    readTable :: JoinTable y c => c (Int,x) y -> c (Int,v,x) y -> c (Int,v,x) y -> c (Int,v,x) y

deriving instance (ArrowTable v c) => ArrowTable v (ValueT val2 c)
deriving instance (Arrow c, Profunctor c, ArrowTable v c) => ArrowTable v (CE.ExceptT e c)
deriving instance (Arrow c, Profunctor c, ArrowTable v c) => ArrowTable v (AE.ExceptT e c)
instance (Arrow c, Profunctor c, Functor f, ArrowTable v c) => ArrowTable v (KleisliT f c) where
    type JoinTable y (KleisliT f c) = JoinTable (f y) c
    readTable f g h = lift $ readTable (unlift f) (unlift g) (unlift h)
instance (Arrow c, ArrowTable v c) => ArrowTable v (StateT s c) where
    type JoinTable y (StateT s c) = JoinTable (s,y) c
    readTable f g h = lift $ proc (s, (ta,ix,x)) ->
        readTable (proc (i,(s,x)) -> unlift f -< (s,(i,x)))
                  (proc (i,v,(s,x)) -> unlift g -< (s,(i,v,x)))
                  (proc (i,v,(s,x)) -> unlift h -< (s,(i,v,x)))
                  -< (ta,ix,(s,x))
instance (Arrow c, ArrowTable v c) => ArrowTable v (ReaderT r c) where
    type JoinTable y (ReaderT r c) = JoinTable y c
    readTable f g h = lift $ proc (r,(ta,ix,x)) ->
        readTable (proc (i,(r,x)) -> unlift f -< (r, (i,x)))
                  (proc (i,v,(r,x)) -> unlift g -< (r, (i,v,x)))
                  (proc (i,v,(r,x)) -> unlift h -< (r, (i,v,x)))
                  -< (ta,ix,(r,x))
instance (ArrowTable v c) => ArrowTable v (WriterT r c) where
  type JoinTable y (WriterT r c) = JoinTable (r,y) c
  readTable = error "TODO: Implement WriterT.readTable"
