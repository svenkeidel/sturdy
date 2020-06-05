{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.LetRec where

import Prelude hiding (lookup,fail,id)

import Control.Arrow.Trans
import Control.Arrow.Transformer.Const
import Control.Arrow.Transformer.Kleisli
import Control.Arrow.Transformer.State
import Control.Arrow.Transformer.Static
import Control.Arrow.Transformer.Writer

import Data.Profunctor


class ArrowLetRec var val c | c -> var, c -> val where
  -- | creates a list of bindings that mutual recursively refer to each other.
  letRec :: c x y -> c ([(var,val)],x) y

------------- Instances --------------
instance ArrowLetRec addr val c => ArrowLetRec addr val (ConstT r c) where
  letRec f = lift $ \r -> letRec (unlift f r)
  {-# INLINE letRec #-}

instance (ArrowLetRec x y c) => ArrowLetRec x y (KleisliT f c) where
  letRec f = lift (letRec (unlift f))
  {-# INLINE letRec #-}

instance (Profunctor c, ArrowLetRec var val c) => ArrowLetRec var val (StateT s c) where
  letRec f = lift $ lmap (\(s,(vs,x)) -> (vs,(s,x))) (letRec (unlift f))
  {-# INLINE letRec #-}

instance (Applicative f, ArrowLetRec addr val c) => ArrowLetRec addr val (StaticT f c) where
  letRec (StaticT f) = StaticT $ letRec <$> f
  {-# INLINE letRec #-}
  {-# SPECIALIZE instance ArrowLetRec var val c => ArrowLetRec var val (StaticT ((->) r) c) #-}

instance ArrowLetRec var val c => ArrowLetRec var val (WriterT w c) where
  letRec f = lift (letRec (unlift f))
  {-# INLINE letRec #-}
