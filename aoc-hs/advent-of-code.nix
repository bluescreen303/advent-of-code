{ mkDerivation, base, comonad, containers, filepath, hashable
, hspec, lib, mtl, parsec, Ranged-sets, unordered-containers
, vector
}:
mkDerivation {
  pname = "advent-of-code";
  version = "2023.0.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    base comonad containers filepath hashable mtl parsec Ranged-sets
    unordered-containers vector
  ];
  executableHaskellDepends = [ base filepath ];
  testHaskellDepends = [ base containers filepath hspec ];
  license = lib.licenses.agpl3Plus;
}
