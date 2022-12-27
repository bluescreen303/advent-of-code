{ mkDerivation, base, hspec, lib }:
mkDerivation {
  pname = "advent-of-code";
  version = "2022.0.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [ base ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base hspec ];
  license = lib.licenses.agpl3Plus;
  mainProgram = "2022-01";
}
