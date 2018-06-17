module SoundnessSpec where

import Soundness
import Syntax

import Data.Error
import Data.GaloisConnection

import Test.Hspec

soundnessSpec :: (Galois vc va,Show vc, Show va) =>
  (Prog -> Error String (rc, pc)) ->
  (Prog -> Error String (ra, pa)) ->
  Spec
soundnessSpec runConcrete runAbstract = do
  sound "$x:=1; $y:=$x" (\(x,y) ->
      [ Assign x (NumLit 1 @@- 1) @@ 1
      , Assign y (Var x @@- 2) @@ 2
      ]) runConcrete runAbstract

  sound "$x:=1; $y:=$z" (\(x,y,z) ->
    [ Assign x (NumLit 1 @@- 1) @@ 1
    , Assign y (Var z @@- 2) @@ 2
    ]) runConcrete runAbstract
