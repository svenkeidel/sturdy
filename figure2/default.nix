{ pkgs ? import <nixpkgs> {} }:

let
  hsEnv = pkgs.haskellPackages.ghcWithPackages(p: with p; [
    Cabal cabal-install containers (p.callPackage ../lib/default.nix { })
  ]);

in pkgs.stdenv.mkDerivation {
  name = "figure2";
  version = "0.0.1";
  src = ./.;
  buildInputs = [
    hsEnv
  ];
}
