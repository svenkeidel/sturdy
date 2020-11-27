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
  -- it "compile id" $ do
  --   toCategory @Primitive (\x -> x) (1 :: Int) `shouldBe` 1

  it "compile tuple" $ do
    toCategory @Primitive (\x -> (x, x)) (1 :: Int) `shouldBe` (1,1)

class Primitive c where
  tuple :: c ctx (x -> y -> (x,y))

instance Primitive (->) where
  tuple ctx x y = (x,y)
