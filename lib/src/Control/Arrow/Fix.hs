module Control.Arrow.Fix
  ( module Control.Arrow.Class.Fix,
    module Control.Arrow.Transformer.FixpointCache
  )
where

import Control.Arrow.Class.Fix
import Control.Arrow.Transformer.FixpointCache

class Arrow c => ArrowFix' c y | c -> y where
  fixA' :: ((z -> c x y) -> (z -> c x y)) -> (z -> c x y)
