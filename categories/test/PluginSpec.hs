{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module PluginSpec where

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Compiling to categories" $
    it "" $ do
      1 `shouldBe` (1 :: Int)
