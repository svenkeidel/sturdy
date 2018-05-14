{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.DefaultError where

import Control.Arrow
import Control.Arrow.Fail

class ArrowFail e c => ArrowDefaultError e c where
  defaultErrorA :: c String e

instance ArrowFail String c => ArrowDefaultError String c where
  defaultErrorA = proc s -> returnA -< s
