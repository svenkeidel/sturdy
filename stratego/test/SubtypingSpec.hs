{-# LANGUAGE OverloadedStrings #-}
module SubtypingSpec(main, spec) where

import           SubtypeRelation as S
import           Sort

import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  let rel = S.insert "B" "A"
          $ S.insert "C" "B"
          $ S.insert "B'" "A"
          $ S.insert "C" "B'"
            S.empty

  it "is a reflexive relation" $ do
    S.subtype rel "A" "A" `shouldBe` True
    S.subtype rel "B" "B" `shouldBe` True
    S.subtype rel "B'" "B'" `shouldBe` True
    S.subtype rel "C" "C" `shouldBe` True
    S.subtype rel Top Top `shouldBe` True
    S.subtype rel Bottom Bottom `shouldBe` True

  it "is a transitive relation" $ do
    S.subtype rel "C" "B" `shouldBe` True
    S.subtype rel "C" "B'" `shouldBe` True
    S.subtype rel "B" "A" `shouldBe` True
    S.subtype rel "B'" "A" `shouldBe` True
    S.subtype rel "C" "A" `shouldBe` True

  it "handles list" $ do
    S.subtype rel (List "C") (List "A") `shouldBe` True
    S.subtype rel (List "C") (List "B") `shouldBe` True
    S.subtype rel (List "C") (List "C") `shouldBe` True
    S.subtype rel (List "C") (List Top) `shouldBe` True
    S.subtype rel (List "C") Top `shouldBe` True
    S.subtype rel (List Top) (List Top) `shouldBe` True
    S.subtype rel (List Top) Top `shouldBe` True
    S.subtype rel Bottom (List Bottom) `shouldBe` True

  it "handles option" $ do
    S.subtype rel (Option "C") (Option "A") `shouldBe` True
    S.subtype rel (Option "C") (Option "B") `shouldBe` True
    S.subtype rel (Option "C") (Option "C") `shouldBe` True
    S.subtype rel (Option "C") (Option Top) `shouldBe` True
    S.subtype rel (Option "C") Top `shouldBe` True
    S.subtype rel (Option Top) (Option Top) `shouldBe` True
    S.subtype rel (Option Top) Top `shouldBe` True
    S.subtype rel Bottom (Option Bottom) `shouldBe` True

  it "handles Tuple" $ do
    S.subtype rel (Tuple ["C","C"]) (Tuple ["A","A"]) `shouldBe` True
    S.subtype rel (Tuple ["B","C"]) (Tuple ["A","B"]) `shouldBe` True
    S.subtype rel (Tuple ["B","C"]) (Tuple ["B","C"]) `shouldBe` True
    S.subtype rel (Tuple ["B","B"]) (Tuple ["B","C"]) `shouldBe` False
    S.subtype rel (Tuple ["B","B"]) (Tuple ["B","B","B"]) `shouldBe` False
    S.subtype rel (Tuple ["A","B"]) (Tuple [Top,"A"]) `shouldBe` True
    S.subtype rel (Tuple ["A","B"]) (Tuple [Top,Top]) `shouldBe` True
    S.subtype rel (Tuple [Top,"B"]) (Tuple [Top,"A"]) `shouldBe` True
    S.subtype rel (Tuple [Top,"B"]) (Tuple [Top,Top]) `shouldBe` True
    S.subtype rel (Tuple [Top,"B"]) (Tuple ["A",Top]) `shouldBe` False

  it "has a smallest element" $ do
    and [S.subtype rel Bottom s | s <- ["A","B","B'","C"]]
      `shouldBe` True
    or [S.subtype rel s Bottom | s <- ["A","B","B'","C"]]
      `shouldBe` False

  it "has a largest element" $ do
    or [S.subtype rel Top s | s <- ["A","B","B'","C"]]
      `shouldBe` False
    and [S.subtype rel s Top | s <- ["A","B","B'","C"]]
      `shouldBe` True
