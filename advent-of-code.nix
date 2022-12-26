{ mkDerivation, base, lib }:
mkDerivation {
  pname = "advent-of-code";
  version = "2022.0.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base ];
  license = lib.licenses.agpl3Plus;
  mainProgram = "advent-of-code";
}
