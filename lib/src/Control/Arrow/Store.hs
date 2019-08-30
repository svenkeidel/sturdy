{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Arrows #-}
module Control.Arrow.Store where

import Prelude hiding (lookup,id,read,fail)

import Control.Arrow
import Control.Arrow.Fail
import Text.Printf
import Data.String
import Data.Profunctor
import GHC.Exts(Constraint)

-- | Arrow-based interface to describe computations that read from a store.
-- The parameter `y` needs to be exposed, because abstract instances
-- may need to join on `y`.
class (Arrow c, Profunctor c) => ArrowStore var val c | c -> var, c -> val where
  type family Join y (c :: * -> * -> *) :: Constraint

  -- | Reads a value from the store. Fails if the binding is not in the current store.
  read :: Join y c => c (val,x) y -> c x y -> c (var,x) y
  -- | Writes a value to the store.
  write :: c (var,val) ()


-- | Simpler version of 'read'
read' :: (Show var, Join val c, IsString e, ArrowFail e c, ArrowStore var val c) => c var val
read' = proc var ->
  read (proc (val,_) -> returnA -< val)
       (proc var     -> fail    -< fromString $ printf "variable %s not bound" (show var))
    -< (var,var)
