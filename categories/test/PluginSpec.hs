{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module PluginSpec where

import Prelude hiding (id,(.),curry)
import Control.CartesianClosedCategory
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "compile id" $ do
    Control.CartesianClosedCategory.toCategory (\x -> x) (1 :: Int) `shouldBe` 1

test = curry @(->) @() @Int @Int
            ((.) @(->) @Int @Int @((), Int)
               (id @(->) @Int)
               (pi2 @(->) @() @Int))
