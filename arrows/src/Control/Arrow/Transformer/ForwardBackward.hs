{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE DataKinds #-}
module Control.Arrow.Transformer.ForwardBackward where

import Prelude hiding (id,(.))

import Control.Category
import Control.Arrow
import Data.Monoidal

import Data.Profunctor.Unsafe (Profunctor(..))
data ForwardBackwardT c x y = ForwardBackwardT
  { forward  :: c x y
  , backward :: c (x,y) x
  }

instance (Profunctor c) => Profunctor (ForwardBackwardT c) where
  dimap f g h = ForwardBackwardT {forward = dimap f g (forward h), backward = _ }

instance (Arrow c, Profunctor c) => Category (ForwardBackwardT c) where
  id = ForwardBackwardT { forward = id, backward = arr fst }
  f . g = ForwardBackwardT
    { forward  = forward f . forward g
    , backward = backward g . second (backward f) . dimap (\(a,c) -> (a,(a,c))) shuffle1 (first (forward g))}
