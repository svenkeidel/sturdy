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
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
module Control.Arrow.Transformer.Reader(Reader(..)) where

import Prelude hiding (id,(.),lookup)

import Control.Arrow
import Control.Arrow.Environment
import Control.Arrow.Fail
import Control.Arrow.Fix
import Control.Arrow.Reader
import Control.Arrow.State
import Control.Arrow.Deduplicate
import Control.Arrow.Try
import Control.Arrow.Lift
import Control.Arrow.Utils
import Control.Category

import Data.Order
import Data.Monoidal

-- Due to "Generalising Monads to Arrows", by John Hughes, in Science of Computer Programming 37.
newtype Reader r c x y = Reader { runReader :: c (r,x) y }

instance ArrowLift (Reader r) where
  lift f = Reader (pi2 >>> f)

instance Arrow c => Category (Reader r c) where
  id = lift id
  Reader f . Reader g = Reader $ (\(r,x) -> (r,(r,x))) ^>> f . second g

instance Arrow c => Arrow (Reader r c) where
  arr f = lift (arr f)
  first (Reader f) = Reader $ (\(r,(b,d)) -> ((r,b),d)) ^>> first f
  second (Reader f) = Reader $ (\(r,(b,d)) -> (b,(r,d))) ^>> second f
  Reader f &&& Reader g = Reader $ f &&& g
  Reader f *** Reader g = Reader $ (\(r,(b,d)) -> ((r,b),(r,d))) ^>> f *** g

instance ArrowChoice c => ArrowChoice (Reader r c) where
  left (Reader f) = Reader $ to distribute ^>> mmap id pi2  ^>> left f
  right (Reader f) = Reader $ to distribute ^>> mmap pi2 id ^>> right f
  Reader f +++ Reader g = Reader (to distribute ^>> f +++ g)
  Reader f ||| Reader g = Reader (to distribute ^>> f ||| g)

instance ArrowApply c => ArrowApply (Reader r c) where
  app = Reader $ (\(r,(Reader f,b)) -> (f,(r,b))) ^>> app

instance Arrow c => ArrowReader r (Reader r c) where
  askA = Reader pi1
  localA (Reader f) = Reader $ (\(_,(r,x)) -> (r,x)) ^>> f

instance ArrowState s c => ArrowState s (Reader r c) where
  getA = lift getA
  putA = lift putA

instance ArrowFail e c => ArrowFail e (Reader r c) where
  failA = lift failA

instance ArrowEnv x y env c => ArrowEnv x y env (Reader r c) where
  lookup = lift lookup
  getEnv = lift getEnv
  extendEnv = lift extendEnv
  localEnv (Reader f) = Reader ((\(r,(env,a)) -> (env,(r,a))) ^>> localEnv f)

instance ArrowFix (r,x) y c => ArrowFix x y (Reader r c) where
  fixA f = Reader (fixA (runReader . f . Reader))

instance ArrowTry (r,x) (r,y) z c => ArrowTry x y z (Reader r c) where
  tryA (Reader f) (Reader g) (Reader h) = Reader $ tryA (pi1 &&& f) g h

instance ArrowZero c => ArrowZero (Reader r c) where
  zeroArrow = lift zeroArrow

instance ArrowPlus c => ArrowPlus (Reader r c) where
  Reader f <+> Reader g = Reader (f <+> g)

instance ArrowDeduplicate c => ArrowDeduplicate (Reader r c) where
  dedupA (Reader f) = Reader (dedupA f)

deriving instance PreOrd (c (r,x) y) => PreOrd (Reader r c x y)
deriving instance LowerBounded (c (r,x) y) => LowerBounded (Reader r c x y)
deriving instance Complete (c (r,x) y) => Complete (Reader r c x y)
deriving instance CoComplete (c (r,x) y) => CoComplete (Reader r c x y)
deriving instance UpperBounded (c (r,x) y) => UpperBounded (Reader r c x y)
