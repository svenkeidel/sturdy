{ mkDerivation, base, containers, hashable, mtl, numeric-limits
, stdenv, text, unordered-containers
}:
mkDerivation {
  pname = "sturdy-lib";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers hashable mtl numeric-limits text
    unordered-containers
  ];
  license = stdenv.lib.licenses.bsd3;
}
