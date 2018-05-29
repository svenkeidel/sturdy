{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.TryCatch where

import Control.Arrow

class Arrow c => ArrowTryCatch e x y c where
  tryCatchA :: c x y -> c (x,e) y -> c x y
