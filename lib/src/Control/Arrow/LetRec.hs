{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.LetRec where

import Prelude hiding (lookup,fail,id)


class ArrowLetRec var val c where
  -- | creates a list of bindings that mutual recursively refer to each other.
  letRec :: c x y -> c ([(var,val)],x) y
