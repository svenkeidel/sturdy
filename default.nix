{ pkgs ? import <nixpkgs> {} }:

let
  hsEnv = pkgs.haskellPackages.ghcWithPackages(p: with p; [
    stack
  ]);

in pkgs.stdenv.mkDerivation {
  name = "sturdy";
  version = "0.0.1";
  src = ./.;
  buildInputs = [
    hsEnv
  ];
}
