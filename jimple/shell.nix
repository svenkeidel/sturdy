{ pkgs ? import <nixpkgs> {} }:

let
  haskellPackagesWithProfiling = pkgs.haskellPackages.override {
    overrides = self: super: {
      mkDerivation = args: super.mkDerivation (args // {
        enableLibraryProfiling = false;
      });
    };
  };

  hsEnv = haskellPackagesWithProfiling.ghcWithPackages(p: with p; [
    Cabal cabal-install hlint text containers hspec mtl numeric-limits criterion fgl
    (p.callPackage ../lib/default.nix { })
  ]);

in pkgs.stdenv.mkDerivation {
  name = "sturdy-jimple";
  version = "0.0.1";
  src = ./.;
  buildInputs = [
    hsEnv
  ];
}
