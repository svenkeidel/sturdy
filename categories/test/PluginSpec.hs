{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MagicHash #-}
module PluginSpec where

import Prelude hiding (id,(.),curry)
import Control.CartesianClosedCategory
import Test.Hspec

main :: IO ()
main = hspec spec


spec :: Spec
spec = do
  it "id" $ do
    toCategory @Primitive (\x -> x) (1 :: Int) `shouldBe` 1

  it "const" $ do
    toCategory @Primitive (\_ -> 1) () `shouldBe` (1 :: Int)
    toCategory @Primitive (\x _ -> x) (1 :: Int) (2 :: Int) `shouldBe` 1
    toCategory @Primitive (\_ y -> y) (1 :: Int) (2 :: Int) `shouldBe` 2

  it "tuple" $ do
    toCategory @Primitive (\x -> (x, x)) (1 :: Int) `shouldBe` (1, 1)
    toCategory @Primitive (\(x, y) -> (y, x)) (1 :: Int, 2 :: Int) `shouldBe` (2, 1)
    toCategory @Primitive (\((x, y), z) -> (x, (y, z))) ((1 :: Int, 2 :: Int), 3 :: Int) `shouldBe` (1,(2,3))

  it "either" $ do
    toCategory @Primitive (\e -> case e of Left x -> x; Right _ -> 2) (Left 1 :: Either Int Bool) `shouldBe` 1

class Primitive c where
  mkTuple :: c ctx (x -> y -> (x,y))
  mkLeft :: c ctx (x -> Either x y)
  mkRight :: c ctx (y -> Either x y)

instance Primitive (->) where
  mkTuple _ x y = (x,y)
  mkLeft _ x = Left x
  mkRight _ y = Right y
