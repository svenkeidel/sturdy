{ mkDerivation, base, containers, hashable, hspec, mtl
, stdenv, text, unordered-containers
}:
mkDerivation {
  pname = "sturdy-lib";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = (if pkgs.stdenv.isDarwin then [
    base containers hashable mtl text unordered-containers
    pkgs.darwin.apple_sdk.frameworks.Cocoa
  ] else [
    base containers hashable mtl text unordered-containers
  ]);
  testHaskellDepends = [ base hspec text unordered-containers ];
  license = stdenv.lib.licenses.bsd3;
}
