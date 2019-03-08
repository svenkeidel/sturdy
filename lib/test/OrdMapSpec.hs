module OrdMapSpec(main, spec) where

import qualified Data.OrdMap as O
import           Test.Hspec
import qualified Data.Abstract.Boolean as A

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "less than" $ do
    it "should be reflexive" $ do
      let o = O.insertLeq "X" ["A"] O.empty
      O.leq "X" ["A"] o `shouldBe` A.True

    it "should compute the upward closure of the comparable set" $ do
      let o = O.insertLeq "X" ["A"] O.empty
      O.leq "X" ["A","B"] o `shouldBe` A.True
      O.leq "X" [] o `shouldBe` A.Top
      O.leq "X" ["B"] o `shouldBe` A.Top

    it "should compute the downward closure of the incomparable set" $ do
      let o = O.insertNotLeq "X" ["A","B"] O.empty
      O.leq "X" ["A"] o `shouldBe` A.False
      O.leq "X" ["B"] o `shouldBe` A.False
      O.leq "X" [] o `shouldBe` A.False

  describe "greater than" $ do
    it "should be reflexive" $ do
      let o = O.insertGeq ["A"] "X" O.empty
      O.geq ["A"] "X" o `shouldBe` A.True

    it "should compute the upward closure of the comparable set" $ do
      let o = O.insertGeq ["A"] "X" O.empty
      O.geq ["A","B"] "X" o `shouldBe` A.True
      O.geq [] "X" o `shouldBe` A.Top
      O.geq ["B"] "X" o `shouldBe` A.Top

    it "should compute the downward closure of the incomparable set" $ do
      let o = O.insertNotGeq ["A","B"] "X" O.empty
      O.geq ["A"] "X" o `shouldBe` A.False
      O.geq ["B"] "X" o `shouldBe` A.False
      O.geq [] "X" o `shouldBe` A.False
