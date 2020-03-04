{ mkDerivation, async, base, bytestring, containers, directory, filepath
, mtl, stdenv, stm, text, time, hspec, QuickCheck
}:
mkDerivation {
  pname = "naive-bayes-classifier";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  executableHaskellDepends = [ base ];
  libraryHaskellDepends = [
    async bytestring containers directory filepath mtl stm text time
  ];
  testHaskellDepends = [
    base hspec QuickCheck
  ];
  doHaddock = false;
  description = "";
  license = stdenv.lib.licenses.mit;
}

