{ mkDerivation, base, bytestring, cassava, containers, diagrams-lib
, diagrams-svg, discrimination, lens, stdenv, text, vector
}:
mkDerivation {
  pname = "examples";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring cassava containers diagrams-lib diagrams-svg
    discrimination lens text vector
  ];
  license = stdenv.lib.licenses.bsd3;
}
