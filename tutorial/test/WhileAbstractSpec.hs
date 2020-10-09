{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
module WhileAbstractSpec where

import           SturdyStyle.AbstractInterpreter
import           Syntax

import qualified Data.HashMap.Lazy as HM
import           Data.Maybe(fromJust)

import           Data.Abstract.Error (Error(..))
import           Data.Abstract.InfiniteNumbers
import           Data.Abstract.Interval (Interval(..))
import qualified Data.Abstract.Map as M
import qualified Data.Abstract.StrongMap as SM
import           Data.Abstract.Terminating (Terminating(..))
import           Data.Abstract.There (There(..))
import           Data.Label

import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "x:=1; y:=1; while(x < 10){y:=x+y; x:=x+1}" $ do
    let ?bound = Interval (-500) 500
    let stmts = [ assign "x" (numLit 1),
                  assign "y" (numLit 1),
                  while (lt (var "x") (numLit 10)) [
                    assign "y" (add (var "x") (var "y")),
                    assign "x" (add (var "x") (numLit 1))
                  ]
                ]
    let (r, SM.Map env) = runWithInitVals 0 [("x", num 1 0), ("y", num 1 0)] stmts
    r `shouldBe`
      (Terminating $ 
        Success (M.fromList [(env HM.! "x", NumVal $ Interval (Number 1) Infinity),
                             (env HM.! "y", NumVal $ Interval (Number 1) Infinity)]))

  it "while true {x:=x+1}" $ do
    let ?bound = Interval (-500) 500
    let stmts = [ while (boolLit True) [ 
                    assign "x" (add (var "x") (numLit 1)) ]
                ]
    let (r, SM.Map env) = runWithInitVals 0 [("x", num 1 1)] stmts
    r `shouldBe` NonTerminating

  it "x:=x+1" $ do
    let ?bound = Interval (-500) 500
    let stmts = [ assign "x" (add (var "x") (numLit 1)) ]
    let (r, SM.Map env) = runWithInitVals 0 [("x", num 10000 Infinity)] stmts
    r `shouldBe`
      (res $ M.fromList [(env HM.! "x", num 10001 Infinity)])

  it "x := 1; while(x < 10){x:= x + 1}" $ do
    let ?bound = Interval (-500) 500
    let stmts = [ assign "x" (numLit 1),
                  while (lt (var "x") (numLit 10))
                    [assign "x" (add (var "x") (numLit 1))]
                ]
    let (r, SM.Map env) = runWithInitVals 10 [("x", num 1 0)] stmts
    r `shouldBe`
      (res $ M.fromList [(env HM.! "x", num 10 10)])

  it "if (x < 1) {y := 1} {y := true} x := x+y" $ do
    let ?bound = Interval (-500) 500
    let stmts = [ if' (lt (var "x") (numLit 1))
                    [ assign "y" (numLit 1) ]
                    [ assign "y" (boolLit True) ],
                  assign "x" (add (var "x") (var "y"))
                ]
    let initVals1 = [("x", num 0 0),("y", num 0 0)]
    let initVals2 = [("x", num 0 1),("y", num 0 0)]
    let initVals3 = [("x", num 1 1),("y", num 0 0)]
    let (r1, SM.Map env1) = runWithInitVals 0 initVals1 stmts
    let (r2, SM.Map env2) = runWithInitVals 0 initVals2 stmts
    let (r3, SM.Map env3) = runWithInitVals 0 initVals3 stmts
    r1 `shouldBe`
      (res $ M.fromList [(env1 HM.! "x", num 1 1), (env1 HM.! "y", num 1 1)])
    --r2 `shouldBe` (res $ M.fromList [])
    --r3 `shouldBe` (res $ M.fromList [])

  where
    res x = Terminating (Success x)
    num i j = NumVal $ Interval i j
--
--  it "x:=1; y:=2" $ do
--    let stmts = [ assign "x" (numLit 1),
--                  assign "y" (numLit 2) ]
--    let (res, env) = runWithInitVals [("x", NumVal 0), ("y", NumVal 0)] stmts
--    res `shouldBe` Success (M.fromList [(env M.! "x", NumVal 1),
--                                        (env M.! "y", NumVal 2)])
--
--  it "x:=0; y:=1; while(y < (z+1)){x:=x+y; y:=y+1}" $ do
--    let stmts = [ assign "x" (numLit 0),
--                  assign "y" (numLit 1),
--                  while (lt (var "y") (add (var "z") (numLit 1))) [
--                    assign "x" (add (var "x") (var "y")),
--                    assign "y" (add (var "y") (numLit 1))
--                  ]
--                ]
--    let env0 = [("x",NumVal 0),("y",NumVal 0),("z",NumVal 10)]
--    let (res, env) = runWithInitVals env0 stmts
--    res `shouldBe` Success (M.fromList [(env M.! "x", NumVal 55),
--                                        (env M.! "y", NumVal 11),
--                                        (env M.! "z", NumVal 10)])
