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
newtype Fix a b x y = Fix (((Store a (Terminating b), Store a (Terminating b)),x) -> (Store a (Terminating b), Terminating y))

runFix :: Fix a b x y -> (x -> Terminating y)
runFix f = runFix' f >>^ snd

runFix' :: Fix a b x y -> (x -> (Store a (Terminating b), Terminating y))
runFix' (Fix f) = (\x -> ((S.empty,S.empty),x)) ^>> f

liftFix :: (x -> y) -> Fix a b x y
liftFix f = Fix ((\((_,o),x) -> (o,x)) ^>> second (f ^>> Terminating))

instance Category (Fix i o) where
  id = liftFix id
  Fix f . Fix g = Fix $ proc ((i,o),x) -> do
    (o',y) <- g -< ((i,o),x)
    case y of
      NonTerminating -> returnA -< (o,NonTerminating)
      Terminating y' -> f -< ((i,o'),y')

instance Arrow (Fix i o) where
  arr f = liftFix (arr f)
  first (Fix f) = Fix $ to assoc ^>> first f >>^ (\((o,x'),y) -> (o,strength1 (x',y)))

instance ArrowChoice (Fix i o) where
  left (Fix f) = Fix $ \((i,o),e) -> case e of
    Left x -> second (fmap Left) (f ((i,o),x))
    Right y -> (o,return (Right y))
  right (Fix f) = Fix $ \((i,o),e) -> case e of
    Left x -> (o,return (Left x))
    Right y -> second (fmap Right) (f ((i,o),y))
  Fix f ||| Fix g = Fix $ \((i,o),e) -> case e of
    Left x -> f ((i,o),x)
    Right y -> g ((i,o),y)

instance ArrowApply (Fix i o) where
  app = Fix $ (\(io,(Fix f,x)) -> (f,(io,x))) ^>> app

#ifdef TRACE
instance (Show x, Show y, Identifiable x, LowerBounded y, Widening y)
  => ArrowFix x y (Fix x y) where
  fixA f = trace (printf "fixA f") $ proc x -> do
    old <- getOutCache -< ()
    setOutCache -< bottom
    y <- localInCache (fix (memoize . f)) -< (old,x)
    new <- getOutCache -< ()
    if new ⊑ old -- We are in the reductive set of `f` and have overshot the fixpoint
    then returnA -< y
    else fixA f -< x

memoize :: (Show x, Show y, Identifiable x, LowerBounded y, Widening y) => Fix x y x y -> Fix x y x y
memoize f = proc x -> do
  m <- lookupOutCache -< trace (printf "\tmemoize -< %s" (show x)) x
  case m of
    Success y -> do
      returnA -< trace (printf "\t%s <- memoize -< %s" (show y) (show x)) y
    Fail _ -> do
      yOld <- lookupInCache -< x
      writeOutCache -< trace (printf "\tout(%s) := %s" (show x) (show (fromError bottom yOld))) (x, fromError bottom yOld)
      y <- f -< trace (printf "\tf -< %s" (show x)) x
      yCached <- lookupOutCache -< x
      updateOutCache -< (x, y)
      yNew <- lookupOutCache -< x
      returnA -< trace (printf "\t%s <- f -< %s\n" (show y) (show x) ++
                        printf "\tout(%s) := %s ▽ %s = %s\n" (show x) (show yCached) (show y) (show yNew) ++
                        printf "\t%s <- memoize -< %s" (show y) (show x)) y
#else
instance (Identifiable x, Widening y)
  => ArrowFix x y (Fix x y) where
  fixA f = proc x -> do
    old <- getOutCache -< ()
    setOutCache -< bottom
    y <- localInCache (fix (memoize . f)) -< (old,x)
    new <- getOutCache -< ()
    if (new ⊑ old) -- We are in the reductive set of `f` and have overshot the fixpoint
    then returnA -< y
    else fixA f -< x

memoize :: (Identifiable x, Widening y) => Fix x y x y -> Fix x y x y
memoize f = proc x -> do
  m <- lookupOutCache -< x
  case m of
    Success y -> do
      returnA -< y
    Fail _ -> do
      yOld <- lookupInCache -< x
      writeOutCache -< (x, yOld)
      y <- catch f -< x
      updateOutCache -< (x, y)
      throw -< y
  where
    catch :: Fix x y a b -> Fix x y a (Terminating b)
    catch (Fix g) = Fix (g >>^ second Terminating)

    throw :: Fix x y (Terminating a) a
    throw = Fix (arr (\((_,o),x) -> (o,x)))
#endif

lookupOutCache :: Identifiable x => Fix x y x (Error () y)
lookupOutCache = Fix $ \((_,o),x) -> (o,strength2 $ S.lookup x o)

lookupInCache :: (Identifiable x, PreOrd y) => Fix x y x (Terminating y)
lookupInCache = Fix $ \((i,o),x) -> (o, return $ fromError bottom $ S.lookup x i)

writeOutCache :: Identifiable x => Fix x y (x,Terminating y) ()
writeOutCache = Fix $ \((_,o),(x,y)) -> (S.insert x y o,return ())

getOutCache :: Fix x y () (Store x (Terminating y))
getOutCache = Fix $ (\((_,o),()) -> (o,return o))

setOutCache :: Fix x y (Store x (Terminating y)) ()
setOutCache = Fix $ (\((_,_),o) -> (o,return ()))

localInCache :: Fix x y x y -> Fix x y (Store x (Terminating y),x) y
localInCache (Fix f) = Fix (\((_,o),(i,x)) -> f ((i,o),x))

updateOutCache :: (Identifiable x, Widening y) => Fix x y (x,Terminating y) ()
updateOutCache = Fix $ \((_,o),(x,y)) -> (S.insertWith (flip (▽)) x y o,return ())

deriving instance (Identifiable a, PreOrd b, PreOrd y) => PreOrd (Fix a b x y)
deriving instance (Identifiable a, Complete b, Complete y) => Complete (Fix a b x y)
deriving instance (Identifiable a, CoComplete b, CoComplete y) => CoComplete (Fix a b x y)
deriving instance (Identifiable a, PreOrd b, PreOrd y) => LowerBounded (Fix a b x y)
-- deriving instance (Identifiable a, UpperBounded b, UpperBounded y) => UpperBounded (Fix a b x y)
