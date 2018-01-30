{-# LANGUAGE OverloadedStrings #-}
module WhileLanguageSoundness where

import Soundness
import WhileLanguage

import Data.Error
import Data.GaloisConnection

import Test.Hspec

whileSoundnessSpec :: (Galois rc ra, Galois pc pa, Show rc, Show ra, Show pc, Show pa) =>
  (Prog -> Error String (rc, pc)) ->
  (Prog -> Error String (ra, pa)) ->
  Spec
whileSoundnessSpec runConcrete runAbstract = do
  sound "$x:=1; $y:=$x" (\(x,y) ->
      [ Assign x (NumLit 1 @@- 1) @@ 1
      , Assign y (Var x @@- 2) @@ 2
      ]) runConcrete runAbstract

  sound "$x:=random; $y:=$x" (\(x,y) ->
      [ Assign x (RandomNum @@- 1) @@ 1
      , Assign y (Var x @@- 2) @@ 2
      ]) runConcrete runAbstract

  sound "$x:=1; $y:=$z" (\(x,y,z) ->
    [ Assign x (NumLit 1 @@- 1) @@ 1
    , Assign y (Var z @@- 2) @@ 2
    ]) runConcrete runAbstract

  sound "$x1:=1; x2:=2; if (random!=0) {$y:=$x1} else {$y:=$x2}" (\() ->
      [ Assign "x1" (NumLit 1 @@- 1) @@ 1
      , Assign "x2" (NumLit 2 @@- 2) @@ 2
      , If (Eq (RandomNum @@- 3) (NumLit 0 @@- 4) @@- 5)
        [Assign "y" (Var "x1" @@- 6) @@ 4]
        [Assign "y" (Var "x2" @@- 7) @@ 5]
        @@ 3
      ]) runConcrete runAbstract

  sound "$x:=1; if (x==2) $y:=3 else $z:=4" (\(x,y,z) ->
    [ Assign x (NumLit 1 @@- 1) @@ 1
    , If (Eq (Var x @@- 2) (NumLit 2 @@- 3) @@- 4)
      [ Assign y (NumLit 3 @@- 5)  @@ 3 ]
      [ Assign z (NumLit 4 @@- 6)  @@ 4 ]
       @@ 2
    ]) runConcrete runAbstract

  sound "$x:=1; if (random==2) $y:=3 else $z:=4" (\(x,y,z) ->
    [ Assign x (NumLit 1 @@- 1) @@ 1
    , If (Eq (RandomNum @@- 2) (NumLit 2 @@- 3) @@- 4)
      [ Assign y (NumLit 3 @@- 5)  @@ 3 ]
      [ Assign z (NumLit 4 @@- 6)  @@ 4 ]
       @@ 2
    ]) runConcrete runAbstract

  sound "$x:=1; if (x==2) $y:=3 else $z:=4; if (x==2) $z:=$y else $y:=$z" (\(x,y,z) ->
    [ Assign x (NumLit 1 @@- 1) @@ 1
    , If (Eq (Var x @@- 2) (NumLit 2 @@- 3) @@- 4)
      [ Assign y (NumLit 3 @@- 5)  @@ 3 ]
      [ Assign z (NumLit 4 @@- 6)  @@ 4 ]
       @@ 2
    , If (Eq (Var x @@- 7) (NumLit 2 @@- 8) @@- 9)
      [ Assign z (Var y @@- 10) @@ 6 ]
      [ Assign y (Var z @@- 11) @@ 7 ]
       @@ 5
    ]) runConcrete runAbstract

  soundProg "$x:=1; y:=2; z:=3; while (x != 100) { x := x + 1; if (x==100) $z:=$y else $y:=$z}" (
    let (x,y,z) = ("x","y","z") in
      [ Assign x (NumLit 1 @@- 1) @@ 1
      , Assign y (NumLit 2 @@- 2) @@ 2
      , Assign z (NumLit 3 @@- 3) @@ 3
      , While (Not (Eq (Var x @@- 4) (NumLit 100 @@- 5) @@- 6)  @@- 7)
        [ Assign x (Add (Var x @@- 8) (NumLit 1 @@- 9) @@- 10) @@ 5
        , If (Eq (Var x @@- 11) (NumLit 101 @@- 12) @@- 13)
          [ Assign z (Var y @@- 14) @@ 7 ]
          [ Assign y (Var z @@- 15) @@ 8 ]
          @@ 6
        ] @@ 4
      ]) runConcrete runAbstract
