{ mkDerivation, base, comonad, containers, ghc-prim, hspec, lib
, mtl, parallel, parsec, vector
}:
mkDerivation {
  pname = "advent-of-code";
  version = "2022.0.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    base comonad containers ghc-prim mtl parallel parsec vector
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base containers hspec ];
  license = lib.licenses.agpl3Plus;
}
