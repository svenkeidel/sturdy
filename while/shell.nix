{ pkgs ? import <nixpkgs> {} }:

let
  hsEnv = pkgs.haskellPackages.ghcWithPackages(p: with p; [
    Cabal cabal-install hlint text containers hspec mtl numeric-limits criterion fgl arrows
    (p.callPackage ../lib/default.nix { })
    (p.callPackage ../rtg/default.nix { })
  ]);

in pkgs.stdenv.mkDerivation {
  name = "sturdy-while";
  version = "0.0.1";
  src = ./.;
  buildInputs = [
    hsEnv
  ];
}
