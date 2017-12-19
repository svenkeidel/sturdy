{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ReadVarsSpec(main, spec) where

import           WhileLanguage
import           ConcreteSemanticsReadVars
import           IntervalAnalysisReadVars
import qualified Soundness as S

import           Test.Hspec
import           Test.QuickCheck

main :: IO ()
main = hspec spec

sound :: (Arbitrary a, Show a) => String -> (a -> Prog) -> Spec
sound desc p = S.sound desc p runConcrete runAbstract propConcrete propAbstract

soundProg :: String -> Prog -> Spec
soundProg desc p = S.soundProg desc p runConcrete runAbstract propConcrete propAbstract

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
  soundProg "$x:=1; y:=2; z:=3; while (x != 100) { x := x + 1; if (x==100) $z:=$y else $y:=$z}" $
    let (x,y,z) = ("x","y","z") in
      [ Assign x (NumLit 1)
      , Assign y (NumLit 2)
      , Assign z (NumLit 3)
      , While (Not $ Eq (Var x) (NumLit 100))
        [ Assign x (Add (Var x) (NumLit 1))
        , If (Eq (Var x) (NumLit 101))
          [ Assign z (Var y) ]
          [ Assign y (Var z) ]
        ]
      ]
