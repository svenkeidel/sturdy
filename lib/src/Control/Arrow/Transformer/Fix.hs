{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
module Control.Arrow.Transformer.Fix where

import Prelude hiding ((.))
import Control.Arrow
import Control.Arrow.Class.Fix
import Control.Category

newtype Fix a b x y = Fix { runFix :: x -> y }
  deriving (Arrow,ArrowChoice,ArrowApply)

deriving instance Category (Fix a b)

instance ArrowFix x y (Fix x y) where
  fixA f = Fix (runFix (f (fixA f)))
