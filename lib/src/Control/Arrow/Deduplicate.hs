module Control.Arrow.Deduplicate where

import Control.Arrow
import Control.Monad.Deduplicate

import Data.Hashable

class Arrow c => ArrowDeduplicate c where
  dedupA :: (Hashable y,Eq y) => c x y -> c x y

instance MonadDeduplicate m => ArrowDeduplicate (Kleisli m) where
  dedupA (Kleisli f) = Kleisli $ \x -> dedup (f x)

instance ArrowDeduplicate (->) where
  dedupA = returnA
