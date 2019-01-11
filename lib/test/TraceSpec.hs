{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
module TraceSpec where

import Prelude hiding (sum)
import Control.Arrow
import Control.Arrow.Fix
import Control.Arrow.Transformer.Concrete.Trace

import Test.Hspec

data Tree = Node Int Tree Tree
          | Leaf Int deriving (Eq)

instance Show Tree where
  show (Node x _ _) = show x
  show (Leaf x) = show x

sum :: (ArrowChoice c, ArrowFix Tree Int c) => c Tree Int
sum = fix $ \sum' -> proc stmts -> case stmts of
  Node x t1 t2 -> do
    s1 <- sum' -< t1
    s2 <- sum' -< t2
    returnA -< x + s1 + s2
  Leaf x -> do
    returnA -< x

spec :: Spec
spec = do
  let x1@(Node _ x2@(Node _ x4 x5) x3@(Node _ x6 x7))
        = Node 1 (Node 2 (Leaf 4) (Leaf 5)) (Node 3 (Leaf 6) (Leaf 7))

  it "summing all nodes in the tree produces the correct trace" $
    let trace :: Log Tree Int
        trace = 
          [ Call x1,
              Call x2,
                Call x4,
                Return 4,
                Call x5,
                Return 5,
              Return 11,
              Call x3,
                Call x6,
                Return 6,
                Call x7,
                Return 7,
              Return 16,
            Return 28
          ]
    in runTraceT sum x1 `shouldBe` (trace,28)

  -- it "Cont Trace" $ do
  --    runTrace (runCont sum) x1 `shouldBe` (trace,28)

  -- it "Trace Cont" $ do
  --    runCont (runTrace sum) x1 `shouldBe` (trace,28)
