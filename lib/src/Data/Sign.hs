module Data.Sign where

import Data.Order

data Sign = Negative | Zero | Positive | Top
  deriving (Show)

instance PreOrd Sign where
  _ ⊑ Top = True
  _ ⊑ _ = False

instance Complete Sign where
  Negative ⊔ Negative = Negative
  Zero ⊔ Zero = Zero
  Positive ⊔ Positive = Positive
  _ ⊔ _ = Top
