{ pkgs ? import <nixpkgs> {} }:

let
  hsEnv = pkgs.haskell.packages.ghc844.ghcWithPackages(p: with p; [
    stack
  ]);

in pkgs.stdenv.mkDerivation {
  name = "sturdy";
  version = "0.0.1";
  src = ./.;
  buildInputs = [
    hsEnv pkgs.pandoc
  ];
}
