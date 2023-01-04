{ mkDerivation, base, comonad, containers, ghc-prim, hspec, lib
, mtl, parsec
}:
mkDerivation {
  pname = "advent-of-code";
  version = "2022.0.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    base comonad containers ghc-prim mtl parsec
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base hspec ];
  license = lib.licenses.agpl3Plus;
}
