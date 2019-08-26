{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module IntervalAnalysisSpec where

import           Prelude hiding (succ,pred)

import           Data.Proxy

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
  return ()

  let ?bound = I.Interval (-100) 100 in
        sharedSpec (\env e -> toEither $ evalInterval (Proxy @3) env e) (NumVal . fromIntegral)

  describe "behavior specific to interval analysis" $ do
    it "should execute both branches on IfZero on interval containing zero" $
      let ?bound = I.Interval (-100) 100
      in evalInterval (Proxy @1) [("x", num (-5) 5)]
          (ifZero "x" (succ zero) (pred zero))
          `shouldBe` Terminating (Success (num (-1) 1))

    it "should compute 0 + -1 + 1 = 0" $
      let ?bound = I.Interval (-100) 100
      in evalInterval (Proxy @1) [] (succ (pred zero)) `shouldBe`
           Terminating (Success (num 0 0))

    it "should analyse addition correctly" $
      let ?bound = I.Interval 0 5
      in do
        evalInterval (Proxy @2) [] (app (app add zero) two) `shouldBe` Terminating (Success (num 2 2))
        evalInterval (Proxy @2) [] (app (app add one) two) `shouldBe` Terminating (Success (num 3 3))

        pendingWith "This test bad: Addition is not defined for negative numbers."
        evalInterval (Proxy @1) [("x", num 0 1)] (app (app add "x") two) `shouldBe` Terminating (Success (num 2 3))

    it "should terminate for the non-terminating program" $
      let ?bound = I.Interval 0 5
      in do evalInterval (Proxy @2) [] (fix (lam "x" "x")) `shouldSatisfy`
              \c -> case c of Terminating (Success (ClosureVal _)) -> True; _ -> False
            evalInterval (Proxy @2) [] (fix (lam "f" (lam "x" (app "f" "x")))) `shouldSatisfy`
              \c -> case c of Terminating (Success (ClosureVal _)) -> True; _ -> False

  where
    add = fix $ lam "add" $ lam "x" $ lam "y" $ ifZero "x" "y" (succ (app (app "add" (pred "x")) "y"))

    one = succ zero
    two = succ one

    num i j = NumVal $ I.Interval i j

    toEither :: Terminating (Error (Pow String) a) -> Either String a
    toEither (Terminating (Fail e)) = Left (unwords (toList e))
    toEither (Terminating (Success x)) = Right x
    toEither NonTerminating = Left "NonTerminating"
