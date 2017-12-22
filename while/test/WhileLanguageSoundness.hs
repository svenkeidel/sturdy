{-# LANGUAGE OverloadedStrings #-}
module WhileLanguageSoundness where

import Soundness
import WhileLanguage

import Data.Error
import Data.GaloisConnection

import Test.Hspec

whileSoundnessSpec :: (Galois rc ra, Galois pc pa, Show ra, Show pa) =>
  (Prog -> Error String rc) ->
  (Prog -> Error String ra) ->
  (Prog -> Error String pc) ->
  (Prog -> Error String pa) ->
  Spec
whileSoundnessSpec runConcrete runAbstract propConcrete propAbstract = do
  sound "$x:=1; $y:=$z" (\(x,y,z) ->
    [ Assign x (NumLit 1)
    , Assign y (Var z)
    ]) runConcrete runAbstract propConcrete propAbstract

  sound "$x:=1; if (x==2) $y:=3 else $z:=4" (\(x,y,z) ->
    [ Assign x (NumLit 1)
    , If (Eq (Var x) (NumLit 2))
      [ Assign y (NumLit 3) ]
      [ Assign z (NumLit 4) ]
    ]) runConcrete runAbstract propConcrete propAbstract

  sound "$x:=1; if (x==2) $y:=3 else $z:=4; if (x==2) $z:=$y else $y:=$z" (\(x,y,z) ->
    [ Assign x (NumLit 1)
    , If (Eq (Var x) (NumLit 2))
      [ Assign y (NumLit 3) ]
      [ Assign z (NumLit 4) ]
    , If (Eq (Var x) (NumLit 2))
      [ Assign z (Var y)]
      [ Assign y (Var z)]
    ]) runConcrete runAbstract propConcrete propAbstract

  soundProg "$x:=1; y:=2; z:=3; while (x != 100) { x := x + 1; if (x==100) $z:=$y else $y:=$z}" (
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
      ]) runConcrete runAbstract propConcrete propAbstract
