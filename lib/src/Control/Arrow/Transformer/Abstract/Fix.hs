{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE CPP #-}
module Control.Arrow.Transformer.Abstract.Fix(Fix,runFix,runFix',liftFix) where

import           Prelude hiding (id,(.),lookup)
import           Data.Function (fix)

import           Control.Arrow
import           Control.Arrow.Fix
import           Control.Arrow.Utils
import           Control.Category

import           Data.Order
import           Data.Identifiable
    
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
newtype Fix a b x y = Fix (((Store a b,Store a b),x) -> (Store a b,y))

runFix :: Fix a b x y -> (x -> y)
runFix f = runFix' f >>^ snd

runFix' :: Fix a b x y -> (x -> (Store a b,y))
runFix' (Fix f) = (\x -> ((S.empty,S.empty),x)) ^>> f

liftFix :: (x -> y) -> Fix a b x y
liftFix f = Fix ((\((_,o),x) -> (o,x)) ^>> second f)

instance Category (Fix i o) where
  id = liftFix id
  Fix f . Fix g = Fix $ proc ((i,o),x) -> do
    (o',y) <- g -< ((i,o),x)
    f -< ((i,o'),y)

instance Arrow (Fix i o) where
  arr f = liftFix (arr f)
  first (Fix f) = Fix $ (\((i,o),(x,y)) -> (((i,o),x),y)) ^>> first f >>^ (\((o,x'),y) -> (o,(x',y)))
  second (Fix f) = Fix $ (\((i,o),(x,y)) -> (x,((i,o),y))) ^>> second f >>^ (\(x,(o,y')) -> (o,(x,y')))

instance ArrowChoice (Fix i o) where
  left (Fix f) = Fix $ (\((i,o),e) -> injectRight (o,injectLeft ((i,o),e))) ^>> left f >>^ eject
  right (Fix f) = Fix $ (\((i,o),e) -> injectRight ((i,o),injectLeft (o,e))) ^>> right f >>^ eject

instance ArrowApply (Fix i o) where
  app = Fix $ (\(io,(Fix f,x)) -> (f,(io,x))) ^>> app

#ifdef TRACE
instance (Show x, Show y, Eq x, Hashable x, LowerBounded y, Widening y)
  => ArrowFix x y (CacheArrow x y) where
  fixA f = trace (printf "fixA f") $ proc x -> do
    old <- getOutCache -< ()
    setOutCache -< bottom
    y <- localInCache (fix (memoize . f)) -< (old,x)
    new <- getOutCache -< ()
    if new ⊑ old -- We are in the reductive set of `f` and have overshot the fixpoint
    then returnA -< y
    else fixA f -< x

memoize :: (Show x, Show y, Eq x, Hashable x, LowerBounded y, Widening y) => CacheArrow x y x y -> CacheArrow x y x y
memoize f = proc x -> do
  m <- lookupOutCache -< trace (printf "\tmemoize -< %s" (show x)) x
  case m of
    Just y -> do
      returnA -< trace (printf "\t%s <- memoize -< %s" (show y) (show x)) y
    Nothing -> do
      yOld <- lookupInCache -< x
      writeOutCache -< trace (printf "\tout(%s) := %s" (show x) (show (fromMaybe bottom yOld))) (x, fromMaybe bottom yOld)
      y <- f -< trace (printf "\tf -< %s" (show x)) x
      yCached <- lookupOutCache -< x
      updateOutCache -< (x, y)
      yNew <- lookupOutCache -< x
      returnA -< trace (printf "\t%s <- f -< %s\n" (show y) (show x) ++
                        printf "\tout(%s) := %s ▽ %s = %s\n" (show x) (show (fromJust yCached)) (show y) (show yNew) ++
                        printf "\t%s <- memoize -< %s" (show y) (show x)) y

#else
instance (Identifiable x, LowerBounded y, Widening y)
  => ArrowFix x y (Fix x y) where
  fixA f = proc x -> do
    old <- getOutCache -< ()
    setOutCache -< bottom
    y <- localInCache (fix (memoize . f)) -< (old,x)
    new <- getOutCache -< ()
    if (new ⊑ old) -- We are in the reductive set of `f` and have overshot the fixpoint
    then returnA -< y
    else fixA f -< x

memoize :: (Identifiable x, LowerBounded y, Widening y) => Fix x y x y -> Fix x y x y
memoize f = proc x -> do
  m <- lookupOutCache -< x
  case m of
    Success y -> do
      returnA -< y
    Fail _ -> do
      yOld <- lookupInCache -< x
      writeOutCache -< (x, fromError bottom yOld)
      y <- f -< x
      updateOutCache -< (x, y)
      returnA -< y
    Bot -> bottom -< ()
#endif

lookupOutCache :: Identifiable x => Fix x y x (Error () y)
lookupOutCache = Fix $ \((_,o),x) -> (o,S.lookup x o)

lookupInCache :: Identifiable x => Fix x y x (Error () y)
lookupInCache = Fix $ \((i,o),x) -> (o,S.lookup x i)

writeOutCache :: Identifiable x => Fix x y (x,y) ()
writeOutCache = Fix $ \((_,o),(x,y)) -> (S.insert x y o,())

getOutCache :: Fix x y () (Store x y)
getOutCache = Fix $ (\((_,o),()) -> (o,o))

setOutCache :: Fix x y (Store x y) ()
setOutCache = Fix $ (\((_,_),o) -> (o,()))

localInCache :: Fix x y x y -> Fix x y (Store x y,x) y
localInCache (Fix f) = Fix (\((_,o),(i,x)) -> f ((i,o),x))

updateOutCache :: (Identifiable x, Widening y) => Fix x y (x,y) ()
updateOutCache = Fix $ \((_,o),(x,y)) -> (S.insertWith (flip (▽)) x y o,())

deriving instance PreOrd (((Store a b,Store a b),x) -> (Store a b,y)) => PreOrd (Fix a b x y)
deriving instance Complete (((Store a b,Store a b),x) -> (Store a b,y)) => Complete (Fix a b x y)
deriving instance CoComplete (((Store a b,Store a b),x) -> (Store a b,y)) => CoComplete (Fix a b x y)
deriving instance LowerBounded (((Store a b,Store a b),x) -> (Store a b,y)) => LowerBounded (Fix a b x y)
deriving instance UpperBounded (((Store a b,Store a b),x) -> (Store a b,y)) => UpperBounded (Fix a b x y)
