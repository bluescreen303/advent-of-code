{ mkDerivation, base, hspec, lib, mtl, parsec }:
mkDerivation {
  pname = "advent-of-code";
  version = "2022.0.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [ base mtl parsec ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base hspec ];
  license = lib.licenses.agpl3Plus;
}
