{-# LANGUAGE ConstraintKinds #-}
module Data.Identifiable where

import Data.Hashable

type Identifiable a = (Eq a, Hashable a)
