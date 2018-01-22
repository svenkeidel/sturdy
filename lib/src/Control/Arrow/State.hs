{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Arrows #-}
module Control.Arrow.State where

import Prelude hiding (id,(.))
import Control.Category
import Control.Arrow
import Control.Arrow.Fail
import Control.Monad.State

class Arrow c => ArrowState s c | c -> s where
  getA :: c () s
  putA :: c s ()

modifyA :: ArrowState s c => c (x,s) s -> c x ()
modifyA f = proc x -> do
  s <- getA -< ()
  putA <<< f -< (x,s)

instance MonadState s m => ArrowState s (Kleisli m) where
  getA = Kleisli (const get)
  putA = Kleisli put

newtype StateArrow s c x y = StateArrow { runStateArrow :: c (s,x) (s,y) }

instance Category c => Category (StateArrow s c) where
  id = StateArrow id
  (StateArrow f) . (StateArrow g) = StateArrow (f . g)

instance Arrow c => Arrow (StateArrow s c) where
  arr f = StateArrow (second (arr f))
  first (StateArrow f) = StateArrow $ proc (s,(x,y)) -> do
    (s',z') <- f -< (s,x)
    returnA -< (s',(z',y))
  second (StateArrow f) = StateArrow $ proc (s,(x,y)) -> do
    (s',z') <- f -< (s,y)
    returnA -< (s',(x,z'))
  StateArrow f *** StateArrow g = StateArrow $ proc (s,(x,y)) -> do
    (s',x') <- f -< (s,x)
    (s'',y') <- g -< (s',y)
    returnA -< (s'',(x',y'))
  StateArrow f &&& StateArrow g = StateArrow $ proc (s,x) -> do
    (s',x') <- f -< (s,x)
    (s'',y') <- g -< (s',x)
    returnA -< (s'',(x',y'))

instance ArrowChoice c => ArrowChoice (StateArrow s c) where
  left (StateArrow f) = StateArrow $ proc (s,e) -> case e of
    Left x -> do
      (s',y) <- f -< (s,x)
      returnA -< (s',Left y)
    Right y ->
      returnA -< (s,Right y)
  right (StateArrow f) = StateArrow $ proc (s,e) -> case e of
    Right x -> do
      (s',y) <- f -< (s,x)
      returnA -< (s',Right y)
    Left y ->
      returnA -< (s,Left y)
  StateArrow f +++ StateArrow g = StateArrow $ proc (s,e) -> case e of
    Left x -> do
      (s',x') <- f -< (s,x)
      returnA -< (s',Left x')
    Right x -> do
      (s',x') <- g -< (s,x)
      returnA -< (s',Right x')
  StateArrow f ||| StateArrow g = StateArrow $ proc (s,e) -> case e of
    Left x -> f -< (s,x)
    Right x -> g -< (s,x)

instance Arrow c => ArrowState s (StateArrow s c) where
  getA = StateArrow (arr (\(a,()) -> (a,a)))
  putA = StateArrow (arr (\(_,s) -> (s,())))

instance ArrowFail e c => ArrowFail e (StateArrow s c) where
  failA = StateArrow $ proc (_,e) -> failA -< e
