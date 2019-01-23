{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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

type Compose (s :: (* -> * -> *) -> (* -> * -> *))
             (t :: (* -> * -> *) -> (* -> * -> *))
             (c :: * -> * -> *)
             (x :: *)
             (y :: *)
  = s (t c) x y

type (:*:) s t c x y = Compose s t c x y
