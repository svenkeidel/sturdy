{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.TryCatch where

import Control.Arrow

class Arrow c => ArrowTryCatch e x y z c where
  tryCatchA :: c x y -> c y z -> c e z -> c x z
