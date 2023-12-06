{ mkDerivation, base, comonad, containers, filepath, hspec, lib
, mtl, parsec, Ranged-sets, vector
}:
mkDerivation {
  pname = "advent-of-code";
  version = "2023.0.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    base comonad containers filepath mtl parsec Ranged-sets vector
  ];
  executableHaskellDepends = [ base filepath ];
  testHaskellDepends = [ base containers filepath hspec ];
  license = lib.licenses.agpl3Plus;
}
