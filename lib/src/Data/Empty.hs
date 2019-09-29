module Data.Empty where

import qualified Data.HashSet as H
import qualified Data.HashMap.Lazy as M

class IsEmpty a where
  empty :: a

instance (IsEmpty a, IsEmpty b) => IsEmpty (a,b) where
  empty = (empty,empty)

instance IsEmpty Int where
  empty = 0

instance IsEmpty (H.HashSet a) where
  empty = H.empty

instance IsEmpty (M.HashMap a b) where
  empty = M.empty

instance IsEmpty [a] where
  empty = []
