{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Control.Arrow.Alloc where

import Control.Arrow

class Arrow c => ArrowAlloc var addr val env store c where
  alloc :: c (var,val,env var addr,store addr val) addr
