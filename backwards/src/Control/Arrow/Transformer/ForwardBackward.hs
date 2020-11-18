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

instance (Arrow c, Profunctor c) => Profunctor (ForwardBackwardT c) where
  dimap f g h =
    ForwardBackwardT
      { forward = dimap f g (forward h)
      , backward =
          -- backward (rmap g (lmap f h))
          -- backward (lmap f h) . lmap (\(a,_) -> (a,a)) (second (forward (lmap f h)))
          -- dimap (\(a,c) -> (a,(f a, c))) fst (second (backward h)) . lmap (\(a, _) -> (a, a)) (second (lmap f (forward h)))
          -- dimap (\(a, c) -> (a, (f a, c))) fst (second (backward h)) . lmap (\(a, _) -> (a, f a)) (second (forward h))
          -- dimap (\((a,b), c) -> (a, (b, c))) fst (second (backward h)) . lmap (\(a, _) -> let b = f a in ((a,b),b)) (second (forward h))
          -- rmap fst (second (backward h)) . lmap (\(a, _) -> let b = f a in (a, (b, b))) (second (second (forward h)))
          dimap (\(a, _) -> let b = f a in (a, (b, b))) fst (second (backward h . second (forward h)))
      }

  lmap f g =
    ForwardBackwardT
      { forward = lmap f (forward g)
      , backward =
          -- backward (g . arr f)
          -- backward (arr f) . second (backward g) . dimap (\(a,c) -> (a,(a,c))) shuffle1 (first (forward (arr f)))
          -- arr fst . second (backward g) . arr (\(a,c) -> (a,(f a, c)))
          dimap (\(a,c) -> (a,(f a, c))) fst (second (backward g))
      }


  rmap f g =
    ForwardBackwardT
      { forward  = forward (arr f) . forward g
      , backward = backward g . lmap (\(a,_) -> (a,a)) (second (forward g))
      }

instance (Arrow c, Profunctor c) => Category (ForwardBackwardT c) where
  id = ForwardBackwardT { forward = id, backward = arr fst }
  f . g = ForwardBackwardT
    { forward  = forward f . forward g
    , backward = backward g . second (backward f) . dimap (\(a,c) -> (a,(a,c))) shuffle1 (first (forward g))
    }

instance (Arrow c, Profunctor c) => Arrow (ForwardBackwardT c) where
  arr f = ForwardBackwardT { forward = arr f, backward = arr fst }
  first = _
