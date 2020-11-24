{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module PluginSpec where

import Control.CartesianClosedCategory(toCategory)
import Test.Hspec

main :: IO ()
main = return (toCategory (\x -> x) ())

spec :: Spec
spec = do
  it "compile id" $ do
    Control.CartesianClosedCategory.toCategory (\x -> x) (1 :: Int) `shouldBe` 1
  
