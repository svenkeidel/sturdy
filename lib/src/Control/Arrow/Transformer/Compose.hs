{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Transformer.Compose where

import Control.Category
import Control.Arrow
import Control.Arrow.Trans
import Control.Arrow.Fix
import Data.Order

newtype IdentityT c x y = IdentityT {runIdentityT :: c x y}
  deriving (Category,Arrow,ArrowChoice,PreOrd,Complete)

instance ArrowTrans IdentityT where
  type Dom1 IdentityT x y = x
  type Cod1 IdentityT x y = y
  lift = IdentityT
  unlift = runIdentityT

instance ArrowApply c => ArrowApply (IdentityT c) where
  app = lift $ first unlift ^>> app

instance ArrowFix x y c => ArrowFix x y (IdentityT c) where
  fix = liftFix

newtype ComposeT
  (s :: (* -> * -> *) -> (* -> * -> *))
  (t :: (* -> * -> *) -> (* -> * -> *))
  (c :: * -> * -> *) (x :: *) (y :: *)
  = ComposeT { runComposeT :: (s (t c)) x y }

type (:*:) = ComposeT

instance (ArrowTrans s,ArrowTrans t) => ArrowTrans (ComposeT s t) where
  type Dom1 (ComposeT s t) x y = Dom1 t (Dom1 s x y) (Cod1 s x y)
  type Cod1 (ComposeT s t) x y = Cod1 t (Dom1 s x y) (Cod1 s x y)
  lift = undefined
  unlift = undefined

(*:) :: ComposeT s t c x y -> s (t c) x y
(*:) = runComposeT
