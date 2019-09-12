-- | Arrow transformer for a dynamic reaching definition analysis.
module Control.Arrow.Transformer.Abstract.ReachingDefinitions
  ( ReachingDefsT
  , R.runReachingDefsT
  ) where

import qualified Control.Arrow.Transformer.ReachingDefinitions as R
import           Data.Abstract.DiscretePowerset

type ReachingDefsT c = R.ReachingDefsT Pow c
