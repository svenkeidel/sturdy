{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ControlFlowGraph where

import Prelude hiding (id,(.))
import WhileLanguage hiding (Assign)

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Arrow
import Control.Category

import Data.Text (Text)
import Data.IntMap (IntMap)
import Data.Map(Map)
import qualified Data.Map as M
import qualified Data.IntMap as IM

data Node = Assign Text Expr
          | Condition Expr
          | Empty
  deriving (Show,Eq)

type Nodes = IntMap Node
data CFG = CFG {exit :: [Int], nodes :: Nodes, edges :: [(Int,Int)]}
  deriving (Show,Eq)
newtype CFGBuilder = CFGBuilder { buildCFG :: Int -> CFG }

instance Eq CFGBuilder where
  b1 == b2 = buildCFG b1 1 == buildCFG b2 1

type Cache = Map Statement Int
newtype M a = M (ReaderT Cache (Writer CFGBuilder) a)
  deriving (Functor,Applicative,Monad,MonadReader Cache,MonadWriter CFGBuilder)

runM :: M a -> Cache -> (a,CFGBuilder)
runM (M m) = runWriter . runReaderT m

execM :: M a -> Cache -> CFGBuilder
execM m = snd . runM m

listenCFG :: M () -> M CFGBuilder
listenCFG m = do
  cache <- ask
  return $ execM (listen m) cache

runCFG :: [Statement] -> CFG
runCFG stmts = buildCFG (execM (runKleisli run stmts) M.empty) 1

fresh :: Int -> Nodes -> Int
fresh i no
  | IM.null no = i
  | otherwise = succ (fst (IM.findMax no))

singleton :: Node -> CFGBuilder
singleton node = CFGBuilder $ \i -> CFG [i] (IM.singleton i node) []

fork :: Expr -> CFGBuilder -> CFGBuilder -> CFGBuilder
fork cond b1 b2 = CFGBuilder $ \i ->
  let en1 = i+1
      en2 = fresh (i+1) no1
      CFG ex1 no1 ed1 = buildCFG b1 en1
      CFG ex2 no2 ed2 = buildCFG b2 en2
  in CFG (ex1 ++ ex2)
         (IM.insert i (Condition cond) (IM.union no1 no2))
         ([(i,en1), (i,en2)] ++ ed1 ++ ed2)

backEdge :: Int -> CFGBuilder
backEdge en = CFGBuilder $ \i ->
  CFG [] (IM.singleton i Empty) [(i,en)]

instance Monoid CFGBuilder where
  mempty = CFGBuilder $ \_ -> CFG [] IM.empty []
  mappend b1 b2
    | b1 == mempty = b2
    | b2 == mempty = b1
    | otherwise = CFGBuilder $ \i ->
      let en1 = i
          en2 = fresh i no1
          CFG ex1 no1 ed1 = buildCFG b1 en1
          CFG ex2 no2 ed2 = buildCFG b2 en2
      in CFG ex2 (IM.union no1 no2) (ed1 ++ [ (ex,en2) | ex <- ex1] ++ ed2)

instance Eval (Kleisli M) Expr where
  lookup = arr Var
  boolLit = arr BoolLit
  and = arr (uncurry And)
  or = arr (uncurry Or)
  not = arr Not
  numLit = arr NumLit
  add = arr (uncurry Add)
  sub = arr (uncurry Sub)
  mul = arr (uncurry Mul)
  div = arr (uncurry Div)
  eq = arr (uncurry Eq)
  fixEval f = f (fixEval f)

instance Run (Kleisli M) Expr where
  fixRun f = Kleisli $ \stmts ->
    if null stmts
    then tell $ singleton Empty
    else forM_ stmts $ \stmt -> do
           cache <- ask
           case M.lookup stmt cache of
             Just entry -> tell (backEdge entry)
             Nothing -> tell $ CFGBuilder $ \i ->
               let cache' = M.insert stmt i cache
               in buildCFG (execM (runKleisli (f (fixRun f)) stmt) cache') i
  store = Kleisli $ \(v,e) -> tell (singleton (Assign v e))
  if_ (Kleisli f) (Kleisli g) = Kleisli $ \(cond,(stmts1,stmts2)) -> do
    b1 <- listenCFG (f stmts1)
    b2 <- listenCFG (g stmts2)
    tell $ fork cond b1 b2
