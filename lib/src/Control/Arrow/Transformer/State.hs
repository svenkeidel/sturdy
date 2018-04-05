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
module Control.Arrow.Transformer.State(State(..),evalState,execState) where

import Prelude hiding (id,(.),lookup,read)

import Control.Arrow
import Control.Arrow.Deduplicate
import Control.Arrow.Environment
import Control.Arrow.Fail
import Control.Arrow.Fix
import Control.Arrow.Lift
import Control.Arrow.Reader
import Control.Arrow.State
import Control.Arrow.Store
import Control.Arrow.Try
import Control.Arrow.Utils
import Control.Category

import Data.Hashable
import Data.Order
import Data.Monoidal

-- Due to "Generalising Monads to Arrows", by John Hughes, in Science of Computer Programming 37.
newtype State s c x y = State { runState :: c (s,x) (s,y) }

evalState :: Arrow c => State s c x y -> c (s,x) y
evalState f = runState f >>> pi2

execState :: Arrow c => State s c x y -> c (s,x) s
execState f = runState f >>> pi1

instance Category c => Category (State s c) where
  id = State id
  State f . State g = State (f . g)

instance ArrowLift (State r) where
  lift f = State (second f)

instance Arrow c => Arrow (State s c) where
  arr f = lift (arr f)
  first (State f) = State $ (\(s,(b,c)) -> ((s,b),c)) ^>> first f >>^ (\((s,d),c) -> (s,(d,c)))
  second (State f) = State $ (\(s,(a,b)) -> (a,(s,b))) ^>> second f >>^ (\(a,(s,c)) -> (s,(a,c)))
  State f &&& State g = State $ (\(s,x) -> ((s,x),x)) ^>> first f >>> (\((s,y),x) -> ((s,x),y)) ^>> first g >>^ (\((s,z),y) -> (s,(y,z)))
  State f *** State g = State $ (\(s,(x,y)) -> ((s,x),y)) ^>> first f >>> (\((s,y),x) -> ((s,x),y)) ^>> first g >>^ (\((s,z),y) -> (s,(y,z)))

instance ArrowChoice c => ArrowChoice (State s c) where
  left (State f) = State (to distribute ^>> left f >>^ from distribute)
  right (State f) = State (to distribute ^>> right f >>^ from distribute)
  State f +++ State g = State $ to distribute ^>> f +++ g >>^ from distribute
  State f ||| State g = State $ to distribute ^>> f ||| g

instance ArrowApply c => ArrowApply (State s c) where
  app = State $ (\(s,(State f,b)) -> (f,(s,b))) ^>> app

instance Arrow c => ArrowState s (State s c) where
  getA = State (arr (\(a,()) -> (a,a)))
  putA = State (arr (\(_,s) -> (s,())))

instance ArrowFail e c => ArrowFail e (State s c) where
  failA = lift failA

instance ArrowReader r c => ArrowReader r (State s c) where
  askA = lift askA
  localA (State f) = State $ (\(s,(r,x)) -> (r,(s,x))) ^>> localA f

instance ArrowEnv x y env c => ArrowEnv x y env (State r c) where
  lookup = lift lookup
  getEnv = lift getEnv
  extendEnv = lift extendEnv
  localEnv (State f) = State ((\(r,(env,a)) -> (env,(r,a))) ^>> localEnv f)

instance ArrowStore var val c => ArrowStore var val (State r c) where
  read = lift read
  write = lift write

instance ArrowFix (s,x) (s,y) c => ArrowFix x y (State s c) where
  fixA f = State (fixA (runState . f . State))

instance ArrowTry (s,x) (s,y) (s,z) c => ArrowTry x y z (State s c) where
  tryA (State f) (State g) (State h) = State $ tryA f g h

instance ArrowZero c => ArrowZero (State s c) where
  zeroArrow = lift zeroArrow

instance ArrowPlus c => ArrowPlus (State s c) where
  State f <+> State g = State (f <+> g)

instance (Eq s, Hashable s, ArrowDeduplicate c) => ArrowDeduplicate (State s c) where
  dedupA (State f) = State (dedupA f)

deriving instance PreOrd (c (s,x) (s,y)) => PreOrd (State s c x y)
deriving instance LowerBounded (c (s,x) (s,y)) => LowerBounded (State s c x y)
deriving instance Complete (c (s,x) (s,y)) => Complete (State s c x y)
deriving instance CoComplete (c (s,x) (s,y)) => CoComplete (State s c x y)
deriving instance UpperBounded (c (s,x) (s,y)) => UpperBounded (State s c x y)
