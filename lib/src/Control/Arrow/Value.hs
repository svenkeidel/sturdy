{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
module Control.Arrow.Value where

import Prelude hiding (fail)

import Control.Arrow
import Control.Arrow.Fail

import Data.Profunctor
import Data.Typeable

import GHC.Exts

import Text.Printf

class IsValue a val | val -> a where
  value :: a -> val

class (ArrowChoice c, Profunctor c, IsValue a val) => ArrowValue a val c where
  type Join y c :: Constraint
  match :: Join y c => c (a,x) y -> c x y -> c (val,x) y

with :: forall a val c e. (Typeable a, Show val, Join val c, ArrowValue a val c, ArrowFail e c, IsString e) => c a a -> c val val
with f = proc val ->
  match (proc (a,_) -> rmap value f -< a)
        (proc val -> fail -< fromString $ printf "Expected %s but got " (show (typeRep (Proxy :: Proxy a))) (show val))
    -< (val,val)
