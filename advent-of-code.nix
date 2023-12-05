{ mkDerivation, base, comonad, containers, filepath, ghc-prim
, hspec, lib, mtl, parallel, parsec, Ranged-sets, vector
}:
mkDerivation {
  pname = "advent-of-code";
  version = "2023.0.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    base comonad containers filepath ghc-prim mtl parallel parsec
    Ranged-sets vector
  ];
  executableHaskellDepends = [ base filepath ];
  testHaskellDepends = [ base containers filepath hspec ];
  license = lib.licenses.agpl3Plus;
}
