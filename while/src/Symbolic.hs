{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Arrows #-}
module Symbolic where

import WhileLanguage
import qualified ConcreteSemantics as Concrete

import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import Data.Order
import Data.GaloisConnection
import Data.Powerset
import Data.Error

import Control.Monad.State hiding (State)
import Control.Monad.Except
import Control.Arrow
import Control.Arrow.Fail

import Debug.Trace

type Val = Pow Expr

type Store = Map Text Val
type Cache = Map Label (Store,Store)
type State = (Store,Cache)

initState :: State
initState = (M.empty,M.empty)


type M = StateT State (Except String)
runM :: [Statement] -> Error String ((),State)
runM ss = fromEither $ runExcept $ runStateT (runKleisli run ss) initState

runAbstract :: [Statement] -> Error String Store
runAbstract ss = fmap (fst.snd) $ runM ss

getStore :: M Store
getStore = get >>= return . fst

putStore :: Store -> M ()
putStore env = modify (\(x,y) -> (env,y))

modifyStore :: (Store -> Store) -> M ()
modifyStore f = modify (\(x,y) -> (f x, y))

putCache :: Cache -> M ()
putCache cache = modify (\(x,y) -> (x,cache))

modifyCache :: (Cache -> Cache) -> M ()
modifyCache f = modify (\(x,y) -> (x, f y))

getCache :: M Cache
getCache = get >>= return . snd

instance Run (Kleisli M) Val where
  fixRun f = Kleisli $ \stmts ->
    forM_ stmts $ \stmt -> do
      cache <- getCache
      now <- getStore
      case M.lookup (label stmt) cache of
        Just (before,after) | before == now -> do
          traceM $ "Cache hit: " ++ show (stmt,before,after)
          putStore after
        _ -> do
          -- recursion hypothesis: if `now` is not changed until next occurrence of `label stmt`, yield `now` as result
          modifyCache (M.insert (label stmt) (now,now))
          traceM $ "Cache miss, cache assumption: " ++ show (stmt,now)
          runKleisli (f (fixRun f)) stmt
          -- recursion step: recursive call on `now` finished, remember that it resulted in `after`
          after <- getStore
          modifyCache (M.insert (label stmt) (now,after))
          traceM $ "Cache store: " ++ show (stmt,now,after)

  store = Kleisli $ \(x,v,_) -> modifyStore (M.insert x v)

  if_ f1 f2 = proc (v,(x,y),_) -> (f1 -< x) ⊔ (f2 -< y)

instance Eval (Kleisli M) Val where
  lookup = arr $ singleton . Var

  boolLit = arr $ singleton . BoolLit
  and = proc (xs,ys) -> returnA -< do
    x <- xs
    y <- ys
    return $ And x y
  or = proc (xs,ys) -> returnA -< do
           x <- xs
           y <- ys
           return $ Or x y
  not = proc xs -> returnA -< do
    x <- xs
    return $ Not x
  numLit = arr $ singleton . NumLit
  add = proc (xs,ys) -> returnA -< do
            x <- xs
            y <- ys
            return $ Add x y
  sub = proc (xs,ys) -> returnA -< do
            x <- xs
            y <- ys
            return $ Sub x y
  mul = proc (xs,ys) -> returnA -< do
            x <- xs
            y <- ys
            return $ Mul x y
  div = proc (xs,ys) -> returnA -< do
            x <- xs
            y <- ys
            return $ Div x y
  eq = proc (xs,ys) -> returnA -< do
           x <- xs
           y <- ys
           return $ Eq x y

  fixEval f = f (fixEval f)
