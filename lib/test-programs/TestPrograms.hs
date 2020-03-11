{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
module TestPrograms where

import           Prelude hiding (lookup,Bounded,Bool(..),fail,not,and)

import           Control.Arrow
import           Control.Arrow.Fix as F
import           Control.Arrow.Fix.Cache
import           Control.Arrow.Fix.Stack
import           Control.Arrow.Order hiding (bottom)

import           Data.Boolean(Logic(..))
import           Data.Abstract.Boolean(Bool(..))
import           Data.Abstract.InfiniteNumbers as Inf
import           Data.Abstract.Interval (Interval)
import qualified Data.Abstract.Interval as I
import           Data.Abstract.Terminating

import           Data.Metric (Metric,Product)
import qualified Data.Metric as M
import           Data.Order
import           Data.Hashable
import           Data.Profunctor
import           Data.Text.Prettyprint.Doc

import           GHC.Generics

fib :: Arr IV IV
fib = fix $ \f ->
  ifLowerThan 0
    (proc _ -> returnA -< I.Interval 0 0)
    (ifLowerThan 1 (proc _ -> returnA -< I.Interval 1 1)
                   (proc n -> do
                      x <- f -< n - I.Interval 1 1
                      y <- f -< n - I.Interval 2 2
                      returnA -< x + y))
{-# INLINE fib #-}

fact :: Arr IV IV
fact = fix $ \f ->
  ifLowerThan 1 (proc _ -> returnA -< iv 1 1)
                (proc n -> do x <- f -< (n - iv 1 1)
                              returnA -< n * x)
{-# INLINE fact #-}

ackermann :: Arr (IV,IV) IV
ackermann = fix $ \f -> proc (m,n) ->
  ifLowerThan 0
    (proc _ -> returnA -< n + iv 1 1)
    (proc m' -> ifLowerThan 0
                  (proc _ -> f -< (m'- iv 1 1, iv 1 1))
                  (proc n' -> do x <- f -< (m,n'-iv 1 1)
                                 f -< (m'- iv 1 1, x)) -<< n)
    -<< m
{-# INLINE ackermann #-}

data EvenOdd = Even | Odd deriving (Eq,Generic,Show,Hashable)
instance Pretty EvenOdd where pretty = viaShow
instance PreOrd EvenOdd where e1 ⊑ e2 = e1 == e2

evenOdd :: Arr (EvenOdd,IV) Bool
evenOdd = fix $ \f -> proc (e,x) -> case e of
  Even -> ifLowerThan 0 (proc _ -> returnA -< true)
                        (ifLowerThan 1 (proc _ -> returnA -< false)
                                       (proc x -> f -< (Odd,x-I.Interval 1 1))) -< x
  Odd -> ifLowerThan 0 (proc _ -> returnA -< false)
                        (ifLowerThan 1 (proc _ -> returnA -< true)
                                       (proc x -> f -< (Even,x-I.Interval 1 1))) -< x
{-# INLINE evenOdd #-}

diverge :: Arr Int IV
diverge = fix $ \f -> proc n -> case n of
  0 -> f -< 0
  _ -> f -< (n-1)
{-# INLINE diverge #-}

data Fun = Main | F | G Bool Bool | H Bool | HF deriving (Eq,Show,Generic,Hashable)
instance Pretty Fun where pretty = viaShow

-- | Example program that demonstrates unsoundness of a fixpoint algorithm without a stack.
-- Pretty printed the program looks like this:
-- main = g(f(x),f(x))
-- f(x) = h(f(x)) ⊔ c
unsoundExample :: Arr Fun Bool
unsoundExample = fix $ \call -> proc fun -> case fun of
  Main -> do
    x <- call -< F
    y <- call -< F
    call -< G x y
  F -> (call -< HF) <⊔> (returnA -< c)
  G x y -> returnA -< y `implies` x
  H x -> returnA -< not x
  HF -> do
    y <- call -< F
    call -< H y
  where
    c = True

type Arr x y = forall c. (?fixpointAlgorithm :: (Fix (c x y) -> Fix (c x y)) -> Fix (c x y),
                          ArrowChoice c, Profunctor c, ArrowApply c, ArrowComplete y c, ArrowFix (c x y))
                          => c x y
newtype Strat x y = Strat { getStrat :: forall c. (ArrowChoice c, Profunctor c, ArrowApply c, ArrowComplete (Terminating y) c, ArrowStack x c, ArrowStackDepth c, ArrowStackElements x c, ArrowCache x (Terminating y) c) => FixpointCombinator c x (Terminating y)}
type IV = Interval (InfiniteNumber Int)

iv :: InfiniteNumber Int -> InfiniteNumber Int -> IV
iv = I.Interval
{-# INLINE iv #-}

ifLowerThan :: (Num n, Ord n, ArrowChoice c, ArrowComplete x c) => n -> c (Interval n) x -> c (Interval n) x -> c (Interval n) x
ifLowerThan l f g = proc x -> case x of
  I.Interval m n
    | n <= l -> f -< x
    | l < m -> g -< x
    | m <= l && l+1 <= n -> (f -< I.Interval m l) <⊔> (g -< I.Interval (l+1) n)
    | otherwise -> f -< I.Interval m l
{-# INLINABLE ifLowerThan #-}

euclid :: Metric IV (Product (InfiniteNumber Int) (InfiniteNumber Int))
euclid = I.metric (Inf.metric M.euclid)
{-# INLINE euclid #-}

