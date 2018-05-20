{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
module Data.Abstract.FreeCompletionComplete where

import Data.Abstract.FreeCompletion
import Data.Order

instance Complete a => Complete (FreeCompletion a) where
  Lower a1 ⊔ Lower a2 = Lower $ a1 ⊔ a2
  _ ⊔ _ = Top
