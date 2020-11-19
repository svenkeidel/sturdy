{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -funfolding-use-threshold=1500 #-}
module Main where

import Prelude hiding (id,(.))

import Criterion
import Criterion.Main

import Data.Profunctor

import Control.DeepSeq
import Control.Category
import Control.Arrow
import Control.Arrow.Transformer.Const
import Control.Arrow.Transformer.Reader
import Control.Arrow.Transformer.State
import Control.Arrow.Transformer.Writer

data Expr = Num Int | Add Expr Expr | Mul Expr Expr
data Val = Val !Int

evalPure :: Expr -> Val
evalPure e = case e of
  Num n -> Val n
  Add e1 e2 ->
    let Val n1 = eval e1
        Val n2 = eval e2
    in Val (n1 + n2)
  Mul e1 e2 ->
    let Val n1 = eval e1
        Val n2 = eval e2
    in Val (n1 * n2)

eval :: (ArrowChoice c) => c Expr Val
eval = proc e -> case e of
  Num n -> returnA -< Val n
  Add e1 e2 -> do
    Val n1 <- eval -< e1
    Val n2 <- eval -< e2
    returnA -< Val (n1 + n2)
  Mul e1 e2 -> do
    Val n1 <- eval -< e1
    Val n2 <- eval -< e2
    returnA -< Val (n1 * n2)

{-# SPECIALIZE eval :: ConstT () (->) Expr Val #-}
{-# SPECIALIZE eval :: ConstT () (ConstT () (->)) Expr Val #-}
{-# SPECIALIZE eval :: ConstT () (ConstT () (ConstT () (->))) Expr Val #-}

{-# SPECIALIZE eval :: ReaderT () (->) Expr Val #-}
{-# SPECIALIZE eval :: ReaderT () (ReaderT () (->)) Expr Val #-}
{-# SPECIALIZE eval :: ReaderT () (ReaderT () (ReaderT () (->))) Expr Val #-}

{-# SPECIALIZE eval :: StateT () (->) Expr Val #-}
{-# SPECIALIZE eval :: StateT () (StateT () (->)) Expr Val #-}
{-# SPECIALIZE eval :: StateT () (StateT () (StateT () (->))) Expr Val #-}

{-# SPECIALIZE eval :: WriterT () (->) Expr Val #-}
{-# SPECIALIZE eval :: WriterT () (WriterT () (->)) Expr Val #-}
{-# SPECIALIZE eval :: WriterT () (WriterT () (WriterT () (->))) Expr Val #-}

{-# SPECIALIZE eval :: ConstT () (ReaderT () (StateT () (->))) Expr Val #-}

addN :: Int -> Expr -> Expr
addN 0 e = e
addN n e = Add (addN (n-1) e) (addN (n-1) e)

main :: IO ()
main = do
  defaultMain
    [
      bgroup "baseline" [ 
        bench "eval pure" $ nf evalPure expr,
        bench "eval (->)" $ nf eval expr
      ],
      bgroup "ConstT" [ 
        bench "ConstT¹" $ nf (runConstT () eval) expr,
        bench "ConstT²" $ nf (runConstT () (runConstT () eval)) expr,
        bench "ConstT³" $ nf (runConstT () (runConstT () (runConstT () eval))) expr 
      ],
      bgroup "ReaderT" [ 
        bench "ReaderT¹" $ nf (runReaderT' eval) expr,
        bench "ReaderT²" $ nf (runReaderT' (runReaderT' eval)) expr,
        bench "ReaderT³" $ nf (runReaderT' (runReaderT' (runReaderT' eval))) expr 
      ],
      bgroup "StateT" [ 
        bench "StateT¹" $ whnf (runStateT' eval) expr,
        bench "StateT²" $ whnf (runStateT' (runStateT' eval)) expr,
        bench "StateT³" $ whnf (runStateT' (runStateT' (runStateT' eval))) expr 
      ],
      bgroup "WriterT" [ 
        bench "WriterT¹" $ nf (runWriterT' eval) expr,
        bench "WriterT²" $ nf (runWriterT' (runWriterT' eval)) expr,
        bench "WriterT³" $ nf (runWriterT' (runWriterT' (runWriterT' eval))) expr 
      ],
      bgroup "Stack" [
        bench "ConstT (ReaderT (StateT (->)))" $
          nf (runStateT' (runReaderT' (runConstT' eval))) expr
      ]
    ]
  where
    runConstT' :: Profunctor c => ConstT () c x y -> c x y
    runConstT' f = runConstT () f
    {-# INLINE runConstT' #-}

    runReaderT' :: Profunctor c => ReaderT () c x y -> c x y
    runReaderT' f = lmap (\x -> ((),x)) (runReaderT f)
    {-# INLINE runReaderT' #-}

    runStateT' :: Profunctor c => StateT () c x y -> c x y
    runStateT' f = dimap (\x -> ((),x)) snd (runStateT f)
    {-# INLINE runStateT' #-}

    runWriterT' :: Profunctor c => WriterT () c x y -> c x y
    runWriterT' f = rmap snd (runWriterT f)
    {-# INLINE runWriterT' #-}

    expr = addN 20 (Num 1)

instance NFData Val where
  rnf (Val n) = rnf n
