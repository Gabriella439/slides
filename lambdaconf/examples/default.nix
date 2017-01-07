{ mkDerivation, async, base, binary, bytestring, http-types
, managed, stdenv, stm, wai, warp
}:
mkDerivation {
  pname = "examples";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    async base binary bytestring http-types managed stm wai warp
  ];
  license = stdenv.lib.licenses.bsd3;
}
