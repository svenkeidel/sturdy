{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Fix(ArrowFix(..),Fix,ArrowFix'(..)) where

import Control.Arrow

type family Fix x y (c :: * -> * -> *) :: * -> * -> *

class Arrow c => ArrowFix x y c where
  fixA :: (c x y -> c x y) -> c x y

instance ArrowFix x y (->) where
  fixA f = f (fixA f)

class Arrow c => ArrowFix' c y | c -> y where
  fixA' :: ((z -> c x y) -> (z -> c x y)) -> (z -> c x y)
