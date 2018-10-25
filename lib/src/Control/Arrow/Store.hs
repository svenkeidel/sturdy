{-# LANGUAGE Arrows                 #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
module Control.Arrow.Store where

import           Prelude            hiding (fail, id, lookup, read)

import           Control.Arrow
import           Control.Arrow.Fail
import           Data.String
import           Text.Printf

-- | Arrow-based interface to describe computations that read from a store.
-- The parameter `y` needs to be exposed, because abstract instances
-- may need to join on `y`.
class Arrow c => ArrowRead var val x y c where
  -- | Reads a value from the store. Fails if the binding is not in the current store.
  read :: c (val,x) y -> c x y -> c (var,x) y

-- | Simpler version of 'read'
read' :: (Show var, IsString e, ArrowFail e c, ArrowRead var val var val c) => c var val
read' = proc var ->
  read (proc (val,_) -> returnA -< val)
       (proc var     -> fail    -< fromString $ printf "variable %s not bound" (show var))
    -< (var,var)

-- | Arrow-based interface to describe computations that modify a store.
class Arrow c => ArrowWrite var val c where
  -- | Writes a value to the store.
  write :: c (var,val) ()

-- | Arrow-based interface to describe computations manipulate a store.
type ArrowStore var val c = (ArrowRead var val var val c, ArrowWrite var val c)
