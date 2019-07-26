-- | Arrow transformer for a dynamic reaching definition analysis.
module Control.Arrow.Transformer.Concrete.ReachingDefinitions
  ( ReachingDefsT
  , R.runReachingDefsT
  ) where

import qualified Control.Arrow.Transformer.ReachingDefinitions as R
import           Data.Functor.Identity
import           Data.Label

type ReachingDefsT c a b = R.ReachingDefsT Identity Label c a b
