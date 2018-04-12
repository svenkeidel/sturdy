{-# LANGUAGE Arrows #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -DTRACE #-}
module Control.Arrow.Transformer.Abstract.Fix(type (~>),runFix,runFix',liftFix) where

import           Prelude hiding (id,(.),lookup)
import           Data.Function (fix)

import           Control.Arrow
import           Control.Arrow.Fix
import           Control.Category

import           Data.Abstract.Terminating
import           Data.Order
import           Data.Identifiable
import           Data.Monoidal

import           Data.Abstract.Error
import           Data.Abstract.Store (Store)
import qualified Data.Abstract.Store as S
import           Data.Abstract.Widening

#ifdef TRACE
import           Debug.Trace
import           Text.Printf
#endif

-- The main idea of this fixpoint caching algorithm is due to David Darais et. al., Abstract Definitional Interpreters (Functional Pearl), ICFP' 17
-- We made some changes to the algorithm to simplify it.

data (~>) x y
type instance Fix a b (~>) = FixArrow a b

newtype FixArrow a b x y = FixArrow (((Store a (Terminating b), Store a (Terminating b)),x) -> (Store a (Terminating b), Terminating y))

runFix :: Fix a b (~>) x y -> (x -> Terminating y)
runFix f = runFix' f >>^ snd

runFix' :: Fix a b (~>) x y -> (x -> (Store a (Terminating b), Terminating y))
runFix' (FixArrow f) = (\x -> ((S.empty,S.empty),x)) ^>> f

liftFix :: (x -> y) -> FixArrow a b x y
liftFix f = FixArrow ((\((_,o),x) -> (o,x)) ^>> second (f ^>> Terminating))

instance Category (FixArrow i o) where
  id = liftFix id
  FixArrow f . FixArrow g = FixArrow $ proc ((i,o),x) -> do
    (o',y) <- g -< ((i,o),x)
    case y of
      NonTerminating -> returnA -< (o,NonTerminating)
      Terminating y' -> f -< ((i,o'),y')

instance Arrow (FixArrow i o) where
  arr f = liftFix (arr f)
  first (FixArrow f) = FixArrow $ to assoc ^>> first f >>^ (\((o,x'),y) -> (o,strength1 (x',y)))

instance ArrowChoice (FixArrow i o) where
  left (FixArrow f) = FixArrow $ \((i,o),e) -> case e of
    Left x -> second (fmap Left) (f ((i,o),x))
    Right y -> (o,return (Right y))
  right (FixArrow f) = FixArrow $ \((i,o),e) -> case e of
    Left x -> (o,return (Left x))
    Right y -> second (fmap Right) (f ((i,o),y))
  FixArrow f ||| FixArrow g = FixArrow $ \((i,o),e) -> case e of
    Left x -> f ((i,o),x)
    Right y -> g ((i,o),y)

instance ArrowLoop (FixArrow i o) where
  loop (FixArrow f) = FixArrow $ loop $ \(((i,o),b),d) ->
    case f ((i,o),(b,d)) of
      (o',Terminating (c,d')) -> ((o',Terminating c),d')
      (o',NonTerminating) -> ((o',NonTerminating),d)

instance ArrowApply (FixArrow i o) where
  app = FixArrow $ (\(io,(FixArrow f,x)) -> (f,(io,x))) ^>> app

#ifdef TRACE
instance (Show x, Show y, Identifiable x, Widening y)
  => ArrowFix x y (FixArrow x y) where
  fixA f = trace (printf "fixA f") $ proc x -> do
    old <- getOutCache -< ()
    setOutCache -< bottom
    y <- localInCache (fix (memoize . f)) -< (old,x)
    new <- getOutCache -< ()
    if new ⊑ old -- We are in the reductive set of `f` and have overshot the fixpoint
    then returnA -< y
    else fixA f -< x

memoize :: (Show x, Show y, Identifiable x, Widening y) => FixArrow x y x y -> FixArrow x y x y
memoize (FixArrow f) = FixArrow $ \((inCache, outCache),x) -> do
  case trace (printf "\tmemoize -< %s" (show x)) (S.lookup x outCache) of
    Success y -> trace (printf "\t%s <- memoize -< %s" (show y) (show x)) (outCache,y)
    Fail _ ->
      let yOld = fromError bottom (S.lookup x inCache)
          outCache' = trace (printf "\tout(%s) := %s" (show x) (show yOld)) (S.insert x yOld outCache)
          (outCache'',y) = trace (printf "\tf -< %s" (show x)) (f ((inCache, outCache'),x))
          outCache''' = S.insertWith (flip (▽)) x y outCache''
      in trace (printf "\t%s <- f -< %s\n" (show y) (show x) ++
                printf "\tout(%s) := %s ▽ %s = %s\n" (show x) (show yOld) (show y) (show (S.lookup x outCache''')) ++
                printf "\t%s <- memoize -< %s" (show y) (show x))
                  (outCache''',y)

#else
instance (Identifiable x, Widening y) => ArrowFix x y (FixArrow x y) where
  fixA f = proc x -> do
    old <- getOutCache -< ()
    setOutCache -< bottom
    y <- localInCache (fix (memoize . f)) -< (old,x)
    new <- getOutCache -< ()
    if (new ⊑ old) -- We are in the reductive set of `f` and have overshot the fixpoint
    then returnA -< y
    else fixA f -< x

memoize :: (Identifiable x, Widening y) => FixArrow x y x y -> FixArrow x y x y
memoize (FixArrow f) = FixArrow $ \((inCache, outCache),x) -> do
  case S.lookup x outCache of
    Success y -> (outCache,y)
    Fail _ ->
      let yOld = fromError bottom (S.lookup x inCache)
          outCache' = S.insert x yOld outCache
          (outCache'',y) = f ((inCache, outCache'),x)
      in (S.insertWith (flip (▽)) x y outCache'',y)
#endif

getOutCache :: FixArrow x y () (Store x (Terminating y))
getOutCache = FixArrow $ (\((_,o),()) -> (o,return o))

setOutCache :: FixArrow x y (Store x (Terminating y)) ()
setOutCache = FixArrow $ (\((_,_),o) -> (o,return ()))

localInCache :: FixArrow x y x y -> FixArrow x y (Store x (Terminating y),x) y
localInCache (FixArrow f) = FixArrow (\((_,o),(i,x)) -> f ((i,o),x))

deriving instance (Identifiable a, PreOrd b, PreOrd y) => PreOrd (FixArrow a b x y)
deriving instance (Identifiable a, Complete b, Complete y) => Complete (FixArrow a b x y)
deriving instance (Identifiable a, CoComplete b, CoComplete y) => CoComplete (FixArrow a b x y)
deriving instance (Identifiable a, PreOrd b, PreOrd y) => LowerBounded (FixArrow a b x y)
-- deriving instance (Identifiable a, UpperBounded b, UpperBounded y) => UpperBounded (Fix a b x y)
