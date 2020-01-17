{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
module IntervalAnalysisSpec where

import           Prelude hiding (succ,pred,id)

import           Data.Abstract.DiscretePowerset(Pow)
import           Data.Abstract.Error hiding (toEither)
import qualified Data.Abstract.Interval as I
import           Data.Abstract.Terminating hiding (toEither)

import           Test.Hspec
import           SharedSpecs

import           IntervalAnalysis

import           Syntax

import           GHC.Exts(toList)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  let ?bound = I.Interval (-100) 100
      ?sensitivity = 3
  sharedSpec (toEither . evalInterval' []) (NumVal . fromIntegral)

  describe "behavior specific to interval analysis" $ do
    it "should execute both branches on IfZero on interval containing zero" $
      let ?bound = I.Interval (-100) 100
          ?sensitivity = 1
      in evalInterval' [("x", num (-5) 5)]
          (ifZero "x" (succ zero) (pred zero))
          `shouldBe` Terminating (Success (num (-1) 1))

    it "should compute 0 + -1 + 1 = 0" $
      let ?bound = I.Interval (-100) 100
          ?sensitivity = 1
      in evalInterval' [] (succ (pred zero)) `shouldBe`
           Terminating (Success (num 0 0))

    it "should analyse addition correctly" $
      let ?bound = I.Interval 0 10
          ?sensitivity = 2
      in do
        evalInterval' [] (let_ [("add",add)] (app "add" [zero,two])) `shouldBe` Terminating (Success (num 2 2))
        evalInterval' [] (let_ [("add",add)] (app "add" [one,two])) `shouldBe` Terminating (Success (num 3 3))
        evalInterval' [("x", num 0 1)] (let_ [("add",add)] (app "add" ["x",two]))
          -- Most precise would be [2,3], however, the analysis does not refine
          -- `x` and therefore introduces some imprecision.
          `shouldBe` Terminating (Success (num 2 7))

    it "context sensitivity" $
      let diamond = let_ [("second",second),("id",id)] (app "second" [app "id" [one],app "id" [two]]) in
      let ?bound = I.Interval 0 5 in do
      let ?sensitivity = 0 in evalInterval' [] diamond `shouldBe` Terminating (Success (num 1 2))
      let ?sensitivity = 1 in evalInterval' [] diamond `shouldBe` Terminating (Success (num 2 2))

    it "should terminate for the non-terminating program" $
      let ?bound = I.Interval 0 5
          ?sensitivity = 2
      in evalInterval' [] (let_ [("id", lam ["x"] "x"),
                                ("fix",lam ["x"] (app "fix" ["x"]))]
                         (app "fix" ["id"]))
           `shouldBe` NonTerminating

  where
    id = lam ["x"] "x"
    second = lam ["_","y"] "y"
    add = lam ["x","y"] $ ifZero "x" "y" (succ (app "add" [pred "x","y"]))

    one = succ zero
    two = succ one

    num i j = NumVal $ I.Interval i j

    toEither :: Terminating (Error (Pow String) a) -> Either String a
    toEither (Terminating (Fail e)) = Left (unwords (toList e))
    toEither (Terminating (Success x)) = Right x
    toEither NonTerminating = Left "NonTerminating"
