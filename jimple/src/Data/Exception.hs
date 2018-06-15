module Data.Exception where

import Data.Order
import Data.String

data Exception v = StaticException String | DynamicException v deriving (Eq)

instance Show v => Show (Exception v) where
  show (StaticException s) = "Static: " ++ s
  show (DynamicException v) = "Dynamic: " ++ show v

instance PreOrd v => PreOrd (Exception v) where
  StaticException s1 ⊑ StaticException s2 = s1 == s2
  _ ⊑ StaticException _ = True
  DynamicException v1 ⊑ DynamicException v2 = v1 ⊑ v2
  _ ⊑ _ = False

instance LowerBounded v => LowerBounded (Exception v) where
  bottom = StaticException "Bottom exception"

instance Complete v => Complete (Exception v) where
  StaticException s1 ⊔ StaticException s2 = StaticException $ s1 ++ "\n" ++ s2
  StaticException s ⊔ _ = StaticException s
  _ ⊔ StaticException s = StaticException s
  DynamicException v1 ⊔ DynamicException v2 = DynamicException $ v1 ⊔ v2

instance IsString (Exception v) where
  fromString = StaticException
