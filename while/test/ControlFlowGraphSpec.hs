{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ControlFlowGraphSpec(main, spec) where

import           Prelude hiding (map)
import qualified Prelude as P

import           WhileLanguage hiding (Assign)
import qualified WhileLanguage as W
import           ControlFlowGraph
import           Data.Monoid
import qualified Data.IntMap as IM

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck hiding (Result(..))

import qualified Data.Text.IO as TIO

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  return ()
--   it "sequential control-flow" $ do
--     let cfg = CFG [2] (IM.fromList [(1,n1),(2,n2)]) [(1,2)]
--     buildCFG (singleton n1 <> singleton n2) 1 `shouldBe` cfg
--     runCFG [s1, s2] `shouldBe` cfg
--
--     buildCFG ((singleton n1 <> singleton n2) <> singleton n4) 1 `shouldBe`
--       buildCFG (singleton n1 <> (singleton n2 <> singleton n4)) 1
--
--     buildCFG (((singleton n1 <> singleton n2) <> fork cond (singleton n4) (singleton n5)) <> singleton n6) 1 `shouldBe`
--       buildCFG ((singleton n1 <> singleton n2) <> (fork cond (singleton n4) (singleton n5) <> singleton n6)) 1
--
--   it "conditional control-flow" $ do
--     runCFG [If cond [s4] [s5]] `shouldBe`
--       buildCFG (fork cond (singleton n4) (singleton n5)) 1
--
--     let cfg = CFG [6] (IM.fromList [(1,n1),(2,n2),(3,Condition cond),(4,n4),(5,n5),(6,n6)]) [(1,2),(2,3),(3,4),(3,5),(4,6),(5,6)]
--     buildCFG (((singleton n1 <> singleton n2) <> fork cond (singleton n4) (singleton n5)) <> singleton n6) 1 `shouldBe` cfg
--     runCFG [s1, s2, If cond [s4] [s5], s6] `shouldBe` cfg
--
--   it "looping control-flow" $ do
--     let cfg = CFG [4] (IM.fromList [(1,Condition cond),(2,n4),(3,Empty),(4,Empty)]) [(1,2),(1,4),(2,3),(3,1)]
--     buildCFG (fork cond (singleton n4 <> backEdge 1) (singleton Empty)) 1 `shouldBe` cfg
--     runCFG [While cond [s4]] `shouldBe` cfg
--
--     let no = IM.fromList [(1,n1),(2,n2),(3,Condition cond),(4,n4),(5,Empty),(6,Empty),(7,n6)]
--         ed = [(1,2),(2,3),(3,4),(3,6),(4,5),(5,3),(6,7)]
--         ex = [7]
--     runCFG [s1, s2, While cond [s4], s6] `shouldBe` CFG ex no ed
--
--   where
--
--     s1 = W.Assign "x" (NumLit 1)
--     s2 = W.Assign "y" (Mul (NumLit 2) (NumLit 1))
--     s4 = W.Assign "z" (Add (Var "x") (NumLit 1))
--     s5 = W.Assign "z" (Add (Var "y") (NumLit 1))
--     s6 = W.Assign "u" (Add (Var "z") (NumLit 1))
--
--     n1 = Assign "x" (NumLit 1)
--     n2 = Assign "y" (Mul (NumLit 2) (NumLit 1))
--     cond = Eq (Var "x") (Var "y")
--     n4 = Assign "z" (Add (Var "x") (NumLit 1))
--     n5 = Assign "z" (Add (Var "y") (NumLit 1))
--     n6 = Assign "u" (Add (Var "z") (NumLit 1))
