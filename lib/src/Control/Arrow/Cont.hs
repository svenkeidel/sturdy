{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Cont where

import Control.Arrow
import Data.Profunctor

class (Arrow c, Profunctor c) => ArrowCont c where
  type Cont c y :: *
  -- | @callCC@ exposes the current continuation. The continuation can be used to escape the current c
  callCC :: (Cont c y -> c x y) -> c x y
  jump :: Cont c x -> c x y
