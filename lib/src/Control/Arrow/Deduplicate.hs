module Control.Arrow.Deduplicate where

import Control.Arrow

import Data.Hashable

class Arrow c => ArrowDeduplicate c where
  dedup :: (Hashable y,Eq y) => c x y -> c x y

instance ArrowDeduplicate (->) where
  dedup = returnA
