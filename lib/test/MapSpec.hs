{-# LANGUAGE OverloadedLists #-}
module MapSpec where

import Prelude hiding (lookup,Maybe(..))

import Data.Order
import Data.Abstract.Maybe
import Data.Abstract.Map
import Data.Abstract.Sign

import Test.Hspec
import Text.Printf

spec :: Spec
spec =
  let m1,m2,m3,m4 :: Map String Sign
      m1 = [("x",Negative), ("y",Positive)]
      m2 = [("x",Positive), ("z",Zero)]
      m3 = [("x",Top),("y",Positive),("z",Zero)]
      m4 = []
  in do

    it "lookup x ([x -> -, y -> +] U [x -> +, z -> 0]) = Just T" $ do
      lookup "x" (m1 ⊔ m2) `shouldBe` Just Top

    it "lookup y ([x -> -, y -> +] U [x -> +, z -> 0]) = JustNothing +" $ do
      lookup "y" (m1 ⊔ m2) `shouldBe` JustNothing Positive

    it "lookup z ([x -> -, y -> +] U [x -> +, z -> 0]) = JustNothing 0" $ do
      lookup "z" (m1 ⊔ m2) `shouldBe` JustNothing Zero

    it "[x -> -, y -> +] <= [x -> -, y -> +] U [x -> +, z -> 0]" $ do
      m1 `shouldBeLessThan` (m1 ⊔ m2)

    it "[x -> +, z -> 0] <= [x -> -, y -> +] U [x -> +, z -> 0]" $ do
      m2 `shouldBeLessThan` (m1 ⊔ m2)

    it "[x -> T, y -> +, z -> 0] <= [x -> -, y -> +] U [x -> +, z -> 0]" $ do
      m3 `shouldBeLessThan` (m1 ⊔ m2)

    it "Map.<= is reflexive" $ do
      m4 `shouldBeLessThan` m4
      m1 `shouldBeLessThan` m1
      m2 `shouldBeLessThan` m2
      m3 `shouldBeLessThan` m3
      (m1 ⊔ m2) `shouldBeLessThan` (m1 ⊔ m2)

  where
    shouldBeLessThan :: (Show a, PreOrd a) => a -> a -> Expectation
    shouldBeLessThan a b
      | a ⊑ b     = return ()
      | otherwise = expectationFailure $ printf "%s </= %s" (show a) (show b)
