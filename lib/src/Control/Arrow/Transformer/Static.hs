{-
Copyright 2004, The University Court of the University of Glasgow. 
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

- Redistributions of source code must retain the above copyright notice,
this list of conditions and the following disclaimer.
 
- Redistributions in binary form must reproduce the above copyright notice,
this list of conditions and the following disclaimer in the documentation
and/or other materials provided with the distribution.
 
- Neither name of the University nor the names of its contributors may be
used to endorse or promote products derived from this software without
specific prior written permission. 

THIS SOFTWARE IS PROVIDED BY THE UNIVERSITY COURT OF THE UNIVERSITY OF
GLASGOW AND THE CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
UNIVERSITY COURT OF THE UNIVERSITY OF GLASGOW OR THE CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
DAMAGE.
-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Control.Arrow.Transformer.Static where

import Prelude hiding (id,(.),lookup,read)

import Control.Category

import Control.Arrow
import Control.Arrow.Environment
import Control.Arrow.Fail
import Control.Arrow.Lift
import Control.Arrow.Reader
import Control.Arrow.State
import Control.Arrow.Store

import Data.Order

-- Due to https://hackage.haskell.org/package/arrows/docs/Control-Arrow-Transformer-Static.html
newtype Static f c x y = Static { runStatic :: f (c x y)}

instance Applicative f => ArrowLift (Static f) where
  lift = Static . pure

instance (Applicative f, Arrow c) => Category (Static f c) where
  id = lift id
  Static f . Static g = Static $ (.) <$> f <*> g

instance (Applicative f, Arrow c) => Arrow (Static f c) where
  arr = lift . arr
  first (Static f) = Static $ first <$> f
  second (Static f) = Static $ second <$> f
  Static f *** Static g = Static $ (***) <$> f <*> g
  Static f &&& Static g = Static $ (&&&) <$> f <*> g

instance (Applicative f, ArrowChoice c) => ArrowChoice (Static f c) where
  left (Static f) = Static $ left <$> f
  right (Static f) = Static $ right <$> f
  Static f +++ Static g = Static $ (+++) <$> f <*> g
  Static f ||| Static g = Static $ (|||) <$> f <*> g

instance (Applicative f, ArrowState s c) => ArrowState s (Static f c) where
  getA = lift getA
  putA = lift putA

instance (Applicative f, ArrowReader r c) => ArrowReader r (Static f c) where
  askA = lift askA
  localA (Static f) = Static $ localA <$> f

instance (Applicative f, ArrowFail e c) => ArrowFail e (Static f c) where
  failA = lift failA

instance (Applicative f, ArrowEnv x y env c) => ArrowEnv x y env (Static f c) where
  lookup = lift lookup
  getEnv = lift getEnv
  extendEnv = lift extendEnv
  localEnv (Static f) = Static $ localEnv <$> f

instance (Applicative f, ArrowStore var val c) => ArrowStore var val (Static f c) where
  read = lift read
  write = lift write

deriving instance PreOrd (f (c x y)) => PreOrd (Static f c x y)
deriving instance Complete (f (c x y)) => Complete (Static f c x y)
deriving instance CoComplete (f (c x y)) => CoComplete (Static f c x y)
deriving instance UpperBounded (f (c x y)) => UpperBounded (Static f c x y)
deriving instance LowerBounded (f (c x y)) => LowerBounded (Static f c x y)
         
