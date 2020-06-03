module UtilsSpec(main, spec) where

import           Control.Arrow
import           Data.List(sort)
import           Data.Utils
import           GHC.Exts
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Powersets" $ do
    it "powComplement [1,2,3]" $
      map (sort *** sort) (toList (powComplement [1,2,3::Int])) `shouldMatchList`
        [ ([1,2,3],[])
        , ([2,3],[1])
        , ([1,3],[2])
        , ([3],[1,2])
        , ([1,2],[3])
        , ([2],[1,3])
        , ([1],[2,3])
        , ([],[1,2,3])
        ]

    it "powComplementN 3 [1,2]" $
      map (map sort) (toList (powComplementN 3 [1,2::Int])) `shouldMatchList`
        [ [[1,2],[],[]]
        , [[1],[2],[]]
        , [[1],[],[2]]
        , [[2],[1],[]]
        , [[2],[],[1]]
        , [[],[1,2],[]]
        , [[],[1],[2]]
        , [[],[2],[1]]
        , [[],[],[1,2]]
        ]

    it "powComplementPick [[A,B],[C,D],[E,F]]" $
      map (map sort) (toList (powComplementPick [["A","B"],["C","D"],["E","F"]])) `shouldMatchList`
        [ [["A","B"],[],[]]
        , [["A"],["D"],[]]
        , [["A"],[],["F"]]
        , [["B"],["C"],[]]
        , [["B"],[],["E"]]
        , [[],["C","D"],[]]
        , [[],["C"],["F"]]
        , [[],["D"],["E"]]
        , [[],[],["E","F"]]
        ]

    it "powComplementPick [[A,B]]" $
      map (map sort) (toList (powComplementPick [["A","B"]])) `shouldMatchList` [[["A","B"]]]

    it "powComplementPick [[]]" $
      toList (powComplementPick [[] :: [Int]]) `shouldMatchList` [[[]]]
