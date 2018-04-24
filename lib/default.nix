{ mkDerivation, base, containers, hashable, hspec, mtl
, stdenv, text, unordered-containers
}:
mkDerivation {
  pname = "sturdy-lib";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers hashable mtl text unordered-containers
  ];
  testHaskellDepends = [ base hspec text unordered-containers ];
  license = stdenv.lib.licenses.bsd3;
}
