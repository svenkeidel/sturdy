{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Arrow.Deduplicate where

import Control.Arrow

import Data.Hashable

class Arrow c => ArrowDeduplicate x y c where
  dedup :: (Hashable y,Eq y) => c x y -> c x y

instance ArrowDeduplicate x y (->) where
  dedup = returnA
