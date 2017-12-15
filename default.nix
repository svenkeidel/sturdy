{ pkgs ? import <nixpkgs> {} }:

let
  hsEnv = pkgs.haskellPackages.ghcWithPackages(p: with p; [
    Cabal cabal-install hlint text containers hspec mtl numeric-limits criterion
  ]);

in pkgs.stdenv.mkDerivation {
  name = "sturdy";
  version = "0.0.1";
  src = ./.;
  buildInputs = [
    hsEnv
  ];
}
