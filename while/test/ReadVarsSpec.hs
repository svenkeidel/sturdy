{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ReadVarsSpec(main, spec) where

import           WhileLanguage
import           ConcreteSemanticsReadVars
import           IntervalAnalysisReadVars
import           Data.Order
import           Data.Text

import           Test.Hspec
import           Test.QuickCheck hiding (Result(..))

main :: IO ()
main = hspec spec

sound :: (Arbitrary a,Show a) => String -> (a -> [Statement]) -> Spec
sound desc genprog = do
  it ("sound value approximation " ++ desc) $ property $ \a -> do
    let prog = genprog a
    let concreteVal = runConcrete prog
    let abstractVal = runAbstract prog
    abstractVal `shouldSatisfy` (concreteVal ⊑)

  it ("sound property approximation " ++ desc) $ property $ \a -> do
    let prog = genprog a
    let concreteProp = propConcrete prog
    let abstractProp = propAbstract prog
    abstractProp `shouldSatisfy` (concreteProp ⊑)

soundProg :: [Statement] -> Spec
soundProg prog = do
  let concreteVal = runConcrete prog
  let abstractVal = runAbstract prog
  it ("sound value approximation " ++ show prog) $ abstractVal `shouldSatisfy` (concreteVal ⊑)

  let concreteProp = propConcrete prog
  let abstractProp = propAbstract prog
  it ("sound property approximation " ++ show prog) $ abstractProp `shouldSatisfy` (concreteProp ⊑)


spec :: Spec
spec = do
  sound "$x:=1; $y:=$z" $ \(x,y,z) -> [Assign x (NumLit 1), Assign y (Var z)]
  sound "$x:=1; if (x==2) $y:=3 else $z:=4" $ \(x,y,z) ->
    [ Assign x (NumLit 1)
    , If (Eq (Var x) (NumLit 2))
      [ Assign y (NumLit 3) ]
      [ Assign z (NumLit 4) ]
    ]
  sound "$x:=1; if (x==2) $y:=3 else $z:=4; if (x==2) $z:=$y else $y:=$z" $ \(x,y,z) ->
    [ Assign x (NumLit 1)
    , If (Eq (Var x) (NumLit 2))
      [ Assign y (NumLit 3) ]
      [ Assign z (NumLit 4) ]
    , If (Eq (Var x) (NumLit 2))
      [ Assign z (Var y)]
      [ Assign y (Var z)]
    ]

p = \(x,y,z) -> [ Assign x (NumLit 1)
        , If (Eq (Var x) (NumLit 2))
          [ Assign y (NumLit 3) ]
          [ Assign z (NumLit 4) ]
        , If (Eq (Var x) (NumLit 2))
          [ Assign z (Var y)]
          [ Assign y (Var z)]
        ]

instance Arbitrary Text where
  arbitrary = fmap pack arbitrary
