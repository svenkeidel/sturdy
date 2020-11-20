{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module PluginSpec where

import Plugin.Categories(toCategory)
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Compiling to categories" $
    it "" $ do
      Plugin.Categories.toCategory (\x -> x) (1 :: Int) `shouldBe` 1
