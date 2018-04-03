{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, containers, hashable, hspec, mtl
      , stdenv, text, unordered-containers, cabal-install
      }:
      mkDerivation {
        pname = "sturdy-lib";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [
          base containers hashable mtl text
          unordered-containers cabal-install
        ];
        testHaskellDepends = [ base hspec text unordered-containers ];
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
